package plan

import (
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"wtprune/git"
	"wtprune/internal/text"
	"wtprune/scan"
)

// Options configure Execute. Zero values mean the real defaults.
type Options struct {
	TrashDir  string             // default DefaultTrashDir()
	LogPath   string             // default DefaultLogPath()
	Cwd       string             // for the re-scan's "current" check (default os.Getwd)
	Processes scan.ProcessLister // for the re-scan's in-use check, listed again per worktree (default lsof)
	Now       func() time.Time   // default time.Now
	Workers   int                // worktrees handled at once (default DefaultWorkers)
}

// DefaultWorkers is how many worktrees Execute handles at once. Removal is
// mostly file deletion, which a single `git worktree remove` does on one
// thread.
const DefaultWorkers = 4

// GitDirName is where a trashed worktree keeps a copy of its admin dir
// (HEAD, reflog, index, in-progress state), inside the trashed dir.
const GitDirName = ".wtprune-gitdir"

// DisarmedGitFile is what a trashed worktree's .git file is renamed to, so
// git run inside the trash can't reach a new worktree that reused its id.
const DisarmedGitFile = ".git.wtprune-orig"

// DefaultTrashDir is ~/.Trash.
func DefaultTrashDir() string {
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".Trash")
}

// DefaultLogPath is $XDG_STATE_HOME/wtprune/log, or ~/.local/state/wtprune/log.
func DefaultLogPath() string {
	if dir := os.Getenv("XDG_STATE_HOME"); dir != "" {
		return filepath.Join(dir, "wtprune", "log")
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".local", "state", "wtprune", "log")
}

// Summary counts what Execute did.
type Summary struct {
	Removed         int
	Trashed         int
	BranchesDeleted int
	Skipped         int // worktrees and branches
	Failed          int
	NotAttempted    int   // left over after cancellation
	Err             error // Execute could not start (eg. log unwritable): nothing was done
	LogPath         string
}

// ExitCode is 1 if anything failed or was left undone, else 0.
func (s Summary) ExitCode() int {
	if s.Failed > 0 || s.NotAttempted > 0 || s.Err != nil {
		return 1
	}
	return 0
}

func (s Summary) String() string {
	if s.Err != nil {
		return s.Err.Error()
	}
	line := fmt.Sprintf("%d removed, %d trashed, %d %s deleted, %d skipped, %d failed",
		s.Removed, s.Trashed, s.BranchesDeleted, text.Plural(s.BranchesDeleted, "branch", "branches"), s.Skipped, s.Failed)
	if s.NotAttempted > 0 {
		line += fmt.Sprintf(", %d not attempted (interrupted)", s.NotAttempted)
	}
	return line + "   log: " + show(s.LogPath)
}

type executor struct {
	opts      Options
	branchFor map[string]*BranchAction // by WorktreeID

	mu  sync.Mutex // guards out, log, sum and Execute's progress counters
	out io.Writer
	log *os.File
	sum Summary

	repos   map[string]*repoState // by CommonDir; the map itself is read-only during the run
	trashMu sync.Mutex            // picking a free Trash name and moving there must not interleave
}

// repoState is one repo's planned branches. Its lock serialises the repo's
// branch checks and deletions and guards abandoned.
type repoState struct {
	mu        sync.Mutex
	branches  []*BranchAction
	abandoned map[*BranchAction]bool // won't be deleted, so it still anchors commits for the others' checks
}

// mayBeDeleted names the planned branches, other than except, that are not
// abandoned: this run may still delete them. Call with mu held. A branch
// only leaves this set by being abandoned, after which it is never deleted,
// so a check that excludes the whole set relies only on refs that survive.
func (r *repoState) mayBeDeleted(except *BranchAction) []string {
	var names []string
	for _, b := range r.branches {
		if b != except && !r.abandoned[b] {
			names = append(names, b.Name)
		}
	}
	return names
}

// Execute runs the plan, a few worktrees at a time (Options.Workers), one
// output line per action, then a summary. Each worktree is re-scanned first
// and skipped if anything changed. Cancelling ctx lets the in-flight
// actions finish, then stops.
func Execute(ctx context.Context, p *Plan, out io.Writer, opts Options) Summary {
	if opts.TrashDir == "" {
		opts.TrashDir = DefaultTrashDir()
	}
	if opts.LogPath == "" {
		opts.LogPath = DefaultLogPath()
	}
	if opts.Processes == nil {
		opts.Processes = scan.ListProcesses
	}
	if opts.Now == nil {
		opts.Now = time.Now
	}
	if opts.Workers <= 0 {
		opts.Workers = DefaultWorkers
	}
	ex := &executor{opts: opts, out: out, branchFor: map[string]*BranchAction{}, repos: map[string]*repoState{}}
	ex.sum.LogPath = opts.LogPath
	items := p.Items()
	repo := func(common string) *repoState {
		if ex.repos[common] == nil {
			ex.repos[common] = &repoState{abandoned: map[*BranchAction]bool{}}
		}
		return ex.repos[common]
	}
	for _, it := range items {
		repo(it.W.Repo.CommonDir)
	}
	for i := range p.Branches {
		b := &p.Branches[i]
		ex.branchFor[b.WorktreeID] = b
		r := repo(b.Repo.CommonDir)
		r.branches = append(r.branches, b)
	}

	log, err := openLog(opts.LogPath)
	if err != nil {
		ex.sum.Err = fmt.Errorf("could not open log %s: %v; nothing done", show(opts.LogPath), err)
		fmt.Fprintln(out, ex.sum.Err)
		return ex.sum
	}
	defer log.Close()
	ex.log = log

	workers := min(opts.Workers, len(items))
	header := fmt.Sprintf("removing %d %s", len(items), text.Plural(len(items), "worktree", "worktrees"))
	if n := len(p.Branches); n > 0 {
		header += fmt.Sprintf(" and %d %s", n, text.Plural(n, "branch", "branches"))
	}
	if workers > 1 {
		header += fmt.Sprintf(", %d at a time", workers)
	}
	fmt.Fprintln(ex.out, header+"...")

	// Actions must not be killed halfway: only check ctx between them.
	actx := context.WithoutCancel(ctx)
	started, inFlight, finished := 0, 0, false
	take := func() (Item, bool) {
		ex.mu.Lock()
		defer ex.mu.Unlock()
		if ctx.Err() != nil || started == len(items) {
			return Item{}, false
		}
		started++
		inFlight++
		return items[started-1], true
	}
	stopNotice := context.AfterFunc(ctx, func() {
		ex.mu.Lock()
		defer ex.mu.Unlock()
		switch {
		case finished:
		case inFlight == 0:
			fmt.Fprintln(ex.out, "interrupted: stopping")
		default:
			fmt.Fprintf(ex.out, "interrupted: finishing %d in-flight %s; Ctrl-C again to abort\n", inFlight, text.Plural(inFlight, "worktree", "worktrees"))
		}
	})
	defer stopNotice()
	var wg sync.WaitGroup
	for range workers {
		wg.Go(func() {
			for it, ok := take(); ok; it, ok = take() {
				ex.worktree(actx, it)
				ex.mu.Lock()
				inFlight--
				ex.mu.Unlock()
			}
		})
	}
	wg.Wait()

	ex.mu.Lock()
	defer ex.mu.Unlock()
	finished = true
	ex.sum.NotAttempted = len(items) - started
	fmt.Fprintln(ex.out, "---")
	fmt.Fprintln(ex.out, ex.sum.String())
	return ex.sum
}

func (ex *executor) worktree(ctx context.Context, it Item) {
	w := it.W
	fresh, warnings, err := scan.Rescan(ctx, w, scan.Options{Cwd: ex.opts.Cwd, Processes: ex.opts.Processes, Now: ex.opts.Now})
	for _, warning := range warnings {
		ex.printf("warning: %s\n", text.Printable(warning))
	}
	if err != nil {
		ex.abandonBranch(w)
		ex.fail(w, "re-scan", err)
		return
	}
	if reason := changedSinceScan(w, fresh); reason != "" {
		ex.abandonBranch(w)
		ex.done(func() { ex.sum.Skipped++ }, "skipped   %s: %s\n", show(w.Path), text.Printable(reason))
		ex.record(logEntry{Action: "skipped", Error: reason}, w)
		return
	}
	lost, more, heldBy, err := ex.lostOnRemoval(ctx, fresh)
	if err != nil {
		ex.abandonBranch(w)
		ex.fail(w, "listing commits removal would lose (nothing removed)", err)
		return
	}
	if more > 0 {
		ex.abandonBranch(w)
		reason := heldOnlyBy(more, heldBy) + "; remove it next run"
		ex.done(func() { ex.sum.Skipped++ }, "skipped   %s: %s\n", show(w.Path), text.Printable(reason))
		ex.record(logEntry{Action: "skipped", Error: reason}, w)
		return
	}

	common := w.Repo.CommonDir
	switch {
	case it.Trash:
		dest, gitdir, failure := ex.trash(w, fresh, lost)
		if failure != nil {
			ex.abandonBranch(w)
			ex.fail(w, failure.what, failure.err)
			return
		}
		if _, err := git.Run(ctx, common, "worktree", "remove", w.Path); err != nil {
			ex.abandonBranch(w)
			ex.fail(w, "moved to "+show(dest)+" but deregistering failed", err)
			return
		}
		ex.done(func() { ex.sum.Trashed++ }, "trashed   %s -> %s\n", show(w.Path), show(dest))
		ex.record(logEntry{Action: "trashed", Trash: dest, GitDir: gitdir, Lost: lost}, fresh)
	case it.Force:
		if _, err := git.Run(ctx, common, "worktree", "remove", "--force", w.Path); err != nil {
			ex.abandonBranch(w)
			ex.fail(w, "", err)
			return
		}
		ex.done(func() { ex.sum.Removed++ }, "removed   %s (forced)\n", show(w.Path))
		ex.record(logEntry{Action: "removed-forced", Lost: lost}, fresh)
	default:
		if err := removeSafely(ctx, common, w.Path); err != nil {
			ex.abandonBranch(w)
			ex.fail(w, "", err)
			return
		}
		ex.done(func() { ex.sum.Removed++ }, "removed   %s\n", show(w.Path))
		ex.record(logEntry{Action: "removed", Lost: lost}, fresh)
	}

	if b := ex.branchFor[w.ID]; b != nil {
		ex.branch(ctx, b, fresh)
	}
}

// stepError is a failed step and what to call it in the FAILED line.
type stepError struct {
	what string
	err  error
}

// trash moves the worktree to a fresh Trash dir and disarms its .git file,
// leaving only the deregistering to the caller. Holding trashMu keeps two
// workers from picking the same free name.
func (ex *executor) trash(w, fresh *scan.Worktree, lost []string) (dest, gitdir string, _ *stepError) {
	ex.trashMu.Lock()
	defer ex.trashMu.Unlock()
	dest, err := ex.trashPath(w)
	if err != nil {
		return "", "", &stepError{"move to Trash (nothing removed)", err}
	}
	gitdir = filepath.Join(dest, GitDirName)
	ex.record(logEntry{Action: "trashing", Trash: dest, GitDir: gitdir, Lost: lost}, fresh)
	if err := moveToTrash(w.Path, fresh.AdminDir, dest); err != nil {
		return "", "", &stepError{"move to Trash (nothing removed)", err}
	}
	// git reuses a freed admin dir id when a worktree with the same name
	// is added again: the trashed .git file must not point at it.
	if err := os.Rename(filepath.Join(dest, ".git"), filepath.Join(dest, DisarmedGitFile)); err != nil {
		return "", "", &stepError{"moved to " + show(dest) + " but not deregistered (could not disarm its .git file)", err}
	}
	return dest, gitdir, nil
}

// lostOnRemoval is lostWith for the repo's planned branches that this run
// may still delete.
func (ex *executor) lostOnRemoval(ctx context.Context, fresh *scan.Worktree) (lost []string, more int, heldBy []string, err error) {
	r := ex.repos[fresh.Repo.CommonDir]
	r.mu.Lock()
	doomed := r.mayBeDeleted(nil)
	r.mu.Unlock()
	return lostWith(ctx, fresh, doomed)
}

// moveToTrash renames the worktree to dest and puts a copy of its admin
// dir in dest/.wtprune-gitdir. On error nothing has moved.
func moveToTrash(path, adminDir, dest string) error {
	partial := partialGitDir(dest)
	if exists(partial) {
		return fmt.Errorf("%s already exists", partial)
	}
	if err := copyDir(adminDir, partial); err != nil {
		os.RemoveAll(partial)
		return fmt.Errorf("copying %s: %w", adminDir, err)
	}
	if err := os.Rename(path, dest); err != nil {
		os.RemoveAll(partial)
		return err
	}
	if err := os.Rename(partial, filepath.Join(dest, GitDirName)); err != nil {
		if undo := os.Rename(dest, path); undo != nil {
			return fmt.Errorf("%w; and moving it back failed, it is at %s: %v", err, dest, undo)
		}
		os.RemoveAll(partial)
		return err
	}
	return nil
}

// copyDir copies src to a new dir dst, keeping modes and symlinks.
func copyDir(src, dst string) error {
	return filepath.WalkDir(src, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(src, p)
		if err != nil {
			return err
		}
		target := filepath.Join(dst, rel)
		info, err := d.Info()
		if err != nil {
			return err
		}
		switch {
		case d.IsDir():
			return os.Mkdir(target, info.Mode().Perm()|0o700)
		case d.Type()&fs.ModeSymlink != 0:
			link, err := os.Readlink(p)
			if err != nil {
				return err
			}
			return os.Symlink(link, target)
		case d.Type().IsRegular():
			return copyFile(p, target, info.Mode().Perm())
		}
		return nil // sockets, fifos: nothing to keep
	})
}

func copyFile(src, dst string, perm fs.FileMode) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_EXCL|os.O_WRONLY, perm)
	if err != nil {
		return err
	}
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}

// removeSafely removes a worktree without --force, so git refuses if it is
// dirty. Repo config status.showUntrackedFiles=no would otherwise make git
// delete untracked files instead of refusing.
func removeSafely(ctx context.Context, common, path string) error {
	_, err := git.Run(ctx, common, "-c", "status.showUntrackedFiles=normal", "worktree", "remove", path)
	return err
}

// changedSinceScan returns why a fresh re-scan no longer matches what the
// user reviewed, or "".
func changedSinceScan(old, fresh *scan.Worktree) string {
	switch {
	case !fresh.Selectable():
		return fmt.Sprintf("changed since scan (now %s)", fresh.State)
	case old.Fingerprint() != fresh.Fingerprint():
		var what []string
		if fresh.Head != old.Head {
			what = append(what, "HEAD moved to "+text.Short(fresh.Head))
		}
		if fresh.Branch.Name != old.Branch.Name {
			what = append(what, "branch is now "+cmp.Or(fresh.Branch.Name, "(detached)"))
		}
		what = append(what, "now "+fresh.StatusText())
		return "changed since scan (" + strings.Join(what, "; ") + ")"
	case !old.AtRisk() && fresh.AtRisk():
		return "changed since scan (now " + strings.Join(fresh.RiskFlags(), " ") + ")"
	case fresh.DetachedLost > old.DetachedLost:
		return fmt.Sprintf("changed since scan (now %d lost commits)", fresh.DetachedLost)
	}
	for _, p := range fresh.InUse {
		if !slices.ContainsFunc(old.InUse, func(q scan.Process) bool { return q.PID == p.PID }) {
			return fmt.Sprintf("changed since scan (now in use by %s, pid %d)", p.Command, p.PID)
		}
	}
	return ""
}

// branch deletes a planned branch after re-checking that every commit on
// its scan-time tip is still contained in a ref that will survive (or, for
// a squash-merged branch, that the squash commit is still in its branch).
//
// It holds the repo's lock throughout: the check excludes the repo's other
// planned branches that are not abandoned at that moment, and a branch is
// only ever abandoned under the same lock and never deleted afterwards, so
// each check relies only on refs nobody here deletes later.
func (ex *executor) branch(ctx context.Context, b *BranchAction, w *scan.Worktree) {
	common := b.Repo.CommonDir
	r := ex.repos[common]
	r.mu.Lock()
	defer r.mu.Unlock()
	where := fmt.Sprintf("branch %s in %s", text.Printable(b.Name), text.Printable(b.Repo.Name))
	reflogOnly := shas(w.ReflogOnly)
	skip := func(reason string) {
		r.abandoned[b] = true
		ex.done(func() { ex.sum.Skipped++ }, "skipped   %s: %s\n", where, text.Printable(reason))
		ex.recordBranch("skipped-branch", b, reason, reflogOnly)
	}
	failed := func(err error) {
		r.abandoned[b] = true
		ex.done(func() { ex.sum.Failed++ }, "FAILED    %s: %s\n", where, text.Printable(err.Error()))
		ex.recordBranch("failed-branch", b, err.Error(), reflogOnly)
	}

	tip, err := git.Run(ctx, common, "rev-parse", "--verify", "--quiet", "refs/heads/"+b.Name)
	if err != nil {
		skip("no longer exists")
		return
	}
	if strings.TrimSpace(tip) != b.Tip {
		skip("tip moved since scan")
		return
	}
	if b.Tier == scan.TierSquashed {
		ok, err := scan.SquashStillHolds(ctx, common, b.Tip, b.SquashCommit, b.SquashRef)
		if err != nil || !ok {
			skip("squash commit no longer in " + strings.TrimPrefix(strings.TrimPrefix(b.SquashRef, "refs/heads/"), "refs/remotes/") + " (or its files differ from the branch)")
			return
		}
	} else {
		exclude := append([]string{b.Name}, r.mayBeDeleted(b)...)
		n, err := scan.CountUnreachableNow(ctx, b.Repo, b.Tip, exclude)
		if err != nil {
			failed(err)
			return
		}
		if n > 0 {
			skip(fmt.Sprintf("%d commits would be lost (contained only in branches being deleted?)", n))
			return
		}
	}
	if _, err := git.Run(ctx, common, "branch", "-D", b.Name); err != nil {
		failed(err)
		return
	}
	ex.done(func() { ex.sum.BranchesDeleted++ }, "deleted   %s (was %s)\n", where, text.Short(b.Tip))
	ex.recordBranch("deleted-branch", b, "", reflogOnly)
}

func (ex *executor) abandonBranch(w *scan.Worktree) {
	if b := ex.branchFor[w.ID]; b != nil {
		r := ex.repos[b.Repo.CommonDir]
		r.mu.Lock()
		defer r.mu.Unlock()
		r.abandoned[b] = true
	}
}

func (ex *executor) fail(w *scan.Worktree, what string, err error) {
	msg := err.Error()
	if what != "" {
		msg = what + ": " + msg
	}
	ex.done(func() { ex.sum.Failed++ }, "FAILED    %s: %s\n", show(w.Path), text.Printable(msg))
	ex.record(logEntry{Action: "failed", Error: msg}, w)
}

// done counts an outcome and prints its line in one step, so the summary
// and the output never disagree.
func (ex *executor) done(count func(), format string, args ...any) {
	ex.mu.Lock()
	defer ex.mu.Unlock()
	count()
	fmt.Fprintf(ex.out, format, args...)
}

func (ex *executor) printf(format string, args ...any) {
	ex.mu.Lock()
	defer ex.mu.Unlock()
	fmt.Fprintf(ex.out, format, args...)
}

// show renders a path for terminal output.
func show(p string) string { return text.Printable(text.Tilde(p)) }

func partialGitDir(dest string) string { return dest + ".gitdir-partial" }

func exists(p string) bool {
	_, err := os.Lstat(p)
	return err == nil
}

// trashPath picks <trash>/wtprune-<name>-<ts>, adding -2, -3... on collision.
func (ex *executor) trashPath(w *scan.Worktree) (string, error) {
	if err := os.MkdirAll(ex.opts.TrashDir, 0o700); err != nil {
		return "", err
	}
	base := fmt.Sprintf("wtprune-%s-%s", filepath.Base(w.Path), ex.opts.Now().Format("20060102-1504"))
	dest := filepath.Join(ex.opts.TrashDir, base)
	for i := 2; ; i++ {
		free := true
		for _, p := range []string{dest, partialGitDir(dest)} {
			_, err := os.Lstat(p)
			if err != nil && !errors.Is(err, fs.ErrNotExist) {
				return "", err
			}
			free = free && err != nil
		}
		if free {
			return dest, nil
		}
		dest = filepath.Join(ex.opts.TrashDir, fmt.Sprintf("%s-%d", base, i))
	}
}

type logEntry struct {
	Time       string   `json:"time"`
	Action     string   `json:"action"`
	Path       string   `json:"path,omitempty"`
	Repo       string   `json:"repo"`
	Head       string   `json:"head,omitempty"`
	Branch     string   `json:"branch,omitempty"`
	Changes    []string `json:"changes,omitempty"`
	Ignored    []string `json:"ignored,omitempty"`
	Trash      string   `json:"trash,omitempty"`
	GitDir     string   `json:"gitdir,omitempty"`      // copy of the admin dir inside Trash
	Lost       []string `json:"lost,omitempty"`        // commits only this worktree's HEAD or in-progress op had
	ReflogOnly []string `json:"reflog_only,omitempty"` // commits only its reflogs had
	Error      string   `json:"error,omitempty"`
}

func (ex *executor) record(e logEntry, w *scan.Worktree) {
	e.Path, e.Repo, e.Head, e.Branch = w.Path, w.Repo.CommonDir, w.Head, w.Branch.Name
	for _, c := range w.Changes {
		e.Changes = append(e.Changes, c.String())
	}
	e.Ignored = slices.Concat(w.IgnoredFiles, w.IgnoredDirs)
	e.ReflogOnly = shas(w.ReflogOnly)
	ex.write(e)
}

func (ex *executor) recordBranch(action string, b *BranchAction, errMsg string, reflogOnly []string) {
	ex.write(logEntry{Action: action, Repo: b.Repo.CommonDir, Head: b.Tip, Branch: b.Name, ReflogOnly: reflogOnly, Error: errMsg})
}

func (ex *executor) write(e logEntry) {
	e.Time = ex.opts.Now().Format(time.RFC3339)
	line, _ := json.Marshal(e)
	ex.mu.Lock()
	defer ex.mu.Unlock()
	if _, err := ex.log.Write(append(line, '\n')); err != nil {
		fmt.Fprintf(ex.out, "warning: action log write failed: %v\n", err)
	}
}

func shas(commits []scan.Commit) []string {
	var out []string
	for _, c := range commits {
		out = append(out, c.SHA)
	}
	return out
}

func openLog(path string) (*os.File, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return nil, err
	}
	return os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
}
