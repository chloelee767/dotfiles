// Package scan discovers linked worktrees (of one repo, or under a folder
// across repos) and inspects each one: changes, risks, blocked states,
// branch deletion tiers and idle time.
package scan

import (
	"cmp"
	"context"
	"fmt"
	"os"
	"os/exec"
	"slices"
	"strings"
	"sync"
	"time"

	"wtprune/git"
)

// workers bounds the concurrent repo loads and worktree inspections.
const workers = 8

// Options configure a Scan. Exactly one of Repo or Dir is set.
type Options struct {
	Repo  string // all linked worktrees of the repo containing this path
	Dir   string // every linked worktree under this path
	Depth int    // max discovery depth for Dir (default 3)

	Cwd       string           // the tool's cwd, for the "current" state (default os.Getwd)
	Processes ProcessLister    // default ListProcesses (lsof)
	Now       func() time.Time // default time.Now
	// Progress is called on entering each phase and after each loaded repo
	// or inspected worktree, never concurrently; may be nil.
	Progress func(Progress)
}

// Phase is a stage of a Scan, for progress output.
type Phase int

const (
	PhaseFinding  Phase = iota // walking --dir for worktrees
	PhaseLoading               // listing each repo's worktrees and refs
	PhaseScanning              // inspecting each worktree
)

// Progress is where a Scan is: Done of Total repos (PhaseLoading) or
// worktrees (PhaseScanning). PhaseFinding has no counts.
type Progress struct {
	Phase       Phase
	Done, Total int
}

// Result is a finished scan.
type Result struct {
	Target   string // --repo: main worktree (common dir if bare); --dir: canonical path
	DirMode  bool
	Rows     []*Worktree // oldest-idle first
	Repos    []*Repo
	Skipped  Skipped // --dir only
	Warnings []string
}

// Scan discovers and inspects worktrees. It errors only when discovery
// itself fails; per-worktree problems become StateError rows.
func Scan(ctx context.Context, opts Options) (*Result, error) {
	if err := CheckGit(); err != nil {
		return nil, err
	}
	opts = withDefaults(opts)
	var progressMu sync.Mutex
	progress := func(p Progress) {
		if opts.Progress != nil {
			progressMu.Lock()
			defer progressMu.Unlock()
			opts.Progress(p)
		}
	}
	var common string
	if opts.Dir == "" {
		// Runs in opts.Repo, which may be a worktree: before lsof starts.
		var err error
		if common, err = commonDirOf(ctx, opts.Repo); err != nil {
			return nil, err
		}
	}
	// lsof and the cwd lookup run alongside discovery, whose git commands
	// run in the repos' common dirs; inspect waits for them before running
	// git inside a worktree.
	procs, cwds := startEnvLookups(ctx, opts)

	res := &Result{}
	var err error
	if opts.Dir != "" {
		res.DirMode = true
		res.Target = Canonical(opts.Dir)
		res.Rows, res.Repos, res.Skipped, err = discoverDir(ctx, opts.Dir, opts.Depth, progress)
	} else {
		res.Rows, res.Repos, err = discoverRepo(ctx, common)
		if err == nil {
			res.Target = cmp.Or(res.Repos[0].MainPath, res.Repos[0].CommonDir)
		}
	}
	if err != nil {
		cwds() // let lsof finish rather than outlive the scan
		return nil, err
	}

	e := newEnv(opts, procs, cwds, res.Repos)
	progress(Progress{Phase: PhaseScanning, Total: len(res.Rows)})
	var wg sync.WaitGroup
	var mu sync.Mutex
	done := 0
	jobs := make(chan *Worktree)
	for range workers {
		wg.Go(func() {
			for w := range jobs {
				inspect(ctx, w, e)
				mu.Lock()
				done++
				progress(Progress{Phase: PhaseScanning, Done: done, Total: len(res.Rows)})
				mu.Unlock()
			}
		})
	}
	for _, w := range res.Rows {
		jobs <- w
	}
	close(jobs)
	wg.Wait()
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	res.Warnings = e.warnings()

	slices.SortStableFunc(res.Rows, func(a, b *Worktree) int {
		if c := a.LastActive.Compare(b.LastActive); c != 0 {
			return c
		}
		return strings.Compare(a.ID, b.ID)
	})
	return res, nil
}

// Rescan re-reads the repo's worktree list and re-inspects one row from
// scratch, for Execute's changed-since-scan check. Only Cwd, Processes and
// Now of opts are used. The branch tier is not re-classified (Execute
// re-checks branches itself), so fresh.Branch only has the name and
// upstream info.
func Rescan(ctx context.Context, w *Worktree, opts Options) (fresh *Worktree, warnings []string, err error) {
	opts = withDefaults(opts)
	procs, cwds := startEnvLookups(ctx, opts)
	fail := func(err error) (*Worktree, []string, error) {
		cwds() // let lsof finish rather than outlive the call
		return nil, nil, err
	}
	repo, err := loadWorktrees(ctx, w.Repo.CommonDir)
	if err != nil {
		// Another worktree of this repo may be mid-removal (Execute runs
		// several at once): its admin dir can vanish under `worktree list`.
		repo, err = loadWorktrees(ctx, w.Repo.CommonDir)
	}
	if err != nil {
		return fail(err)
	}
	// Without the branch tier, the default branches only serve as cheap
	// proofs, checked against their current values: the scan's list will do.
	repo.protected, repo.defaultBranches = w.Repo.protected, w.Repo.defaultBranches
	reg, ok := repo.registrationAt(w.ID)
	if !ok {
		return fail(fmt.Errorf("%s is no longer a registered worktree of %s", w.Path, repo.CommonDir))
	}
	fresh = registeredRow(repo, reg)
	e := newEnv(opts, procs, cwds, []*Repo{repo})
	e.rescan = true
	inspect(ctx, fresh, e)
	return fresh, e.warnings(), nil
}

// CheckGit fails when git is not on PATH.
func CheckGit() error {
	if _, err := exec.LookPath("git"); err != nil {
		return fmt.Errorf("git not found on PATH: %w", err)
	}
	return nil
}

func withDefaults(opts Options) Options {
	if opts.Depth == 0 {
		opts.Depth = 3
	}
	if opts.Cwd == "" {
		opts.Cwd, _ = os.Getwd()
	}
	if opts.Processes == nil {
		opts.Processes = ListProcesses
	}
	if opts.Now == nil {
		opts.Now = time.Now
	}
	return opts
}

// startEnvLookups starts listing processes and resolving the cwd in the
// background; the returned funcs wait for the results. Until procs returns,
// run git only outside the worktrees (eg. -C the common dir).
func startEnvLookups(ctx context.Context, opts Options) (procs func() ([]Process, error), cwds func() []string) {
	procs = sync.OnceValues(func() ([]Process, error) {
		list, err := opts.Processes(ctx)
		return canonicalProcs(list), err
	})
	cwds = sync.OnceValue(func() []string {
		procs() // its git runs in the cwd, which may be a worktree
		return cwdPaths(ctx, opts.Cwd)
	})
	go cwds()
	return procs, cwds
}

func newEnv(opts Options, procs func() ([]Process, error), cwds func() []string, repos []*Repo) env {
	return env{
		cwds:       cwds,
		procs:      procs,
		registered: registeredPaths(repos),
		now:        opts.Now(),
	}
}

// cwdPaths returns the canonical cwd plus git's own toplevel for it, since
// $PWD-derived paths can differ in symlinks and case.
func cwdPaths(ctx context.Context, cwd string) []string {
	if cwd == "" {
		return nil
	}
	paths := []string{Canonical(cwd)}
	if top, err := git.Run(ctx, cwd, "rev-parse", "--show-toplevel"); err == nil && strings.TrimSpace(top) != "" {
		paths = append(paths, Canonical(strings.TrimSpace(top)))
	}
	return paths
}

// canonicalProcs drops this process and resolves each cwd once.
func canonicalProcs(procs []Process) []Process {
	self := os.Getpid()
	cache := map[string]string{}
	var out []Process
	for _, p := range procs {
		if p.PID == self || p.Cwd == "" {
			continue
		}
		c, ok := cache[p.Cwd]
		if !ok {
			c = Canonical(p.Cwd)
			cache[p.Cwd] = c
		}
		p.Cwd = c
		out = append(out, p)
	}
	return out
}
