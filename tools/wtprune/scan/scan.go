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

const workers = 8

// Options configure a Scan. Exactly one of Repo or Dir is set.
type Options struct {
	Repo  string // all linked worktrees of the repo containing this path
	Dir   string // every linked worktree under this path
	Depth int    // max discovery depth for Dir (default 3)

	Cwd       string                // the tool's cwd, for the "current" state (default os.Getwd)
	Processes ProcessLister         // default ListProcesses (lsof)
	Now       func() time.Time      // default time.Now
	Progress  func(done, total int) // called after each inspected worktree; may be nil
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
	res := &Result{}
	var err error
	if opts.Dir != "" {
		res.DirMode = true
		res.Target = Canonical(opts.Dir)
		res.Rows, res.Repos, res.Skipped, err = discoverDir(ctx, opts.Dir, opts.Depth)
	} else {
		res.Rows, res.Repos, err = discoverRepo(ctx, opts.Repo)
		if err == nil {
			res.Target = cmp.Or(res.Repos[0].MainPath, res.Repos[0].CommonDir)
		}
	}
	if err != nil {
		return nil, err
	}

	e, warnings := newEnv(ctx, opts, res.Repos)
	res.Warnings = warnings

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
				if opts.Progress != nil {
					opts.Progress(done, len(res.Rows))
				}
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
// Now of opts are used.
func Rescan(ctx context.Context, w *Worktree, opts Options) (fresh *Worktree, warnings []string, err error) {
	repo, err := loadRepo(ctx, w.Repo.CommonDir)
	if err != nil {
		return nil, nil, err
	}
	reg, ok := repo.registrationAt(w.ID)
	if !ok {
		return nil, nil, fmt.Errorf("%s is no longer a registered worktree of %s", w.Path, repo.CommonDir)
	}
	fresh = registeredRow(repo, reg)
	e, warnings := newEnv(ctx, withDefaults(opts), []*Repo{repo})
	inspect(ctx, fresh, e)
	return fresh, warnings, nil
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

func newEnv(ctx context.Context, opts Options, repos []*Repo) (env, []string) {
	var warnings []string
	procs, err := opts.Processes(ctx)
	if err != nil {
		warnings = append(warnings, fmt.Sprintf("in-use detection unavailable: %v", err))
	}
	return env{
		cwds:       cwdPaths(ctx, opts.Cwd),
		procs:      canonicalProcs(procs),
		registered: registeredPaths(repos),
		now:        opts.Now(),
	}, warnings
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
