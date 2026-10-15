package scan

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"

	"wtprune/git"
)

// env is the per-run context shared by every inspection.
type env struct {
	cwds       []string // canonical: the tool's cwd and its git toplevel
	procs      []Process
	registered []string // canonical paths of all known worktrees
	now        time.Time
}

// reflogDepth caps how many entries of each reflog are checked for
// reflog-only commits.
const reflogDepth = 500

// inspect fills in status, risks, blocked state, branch tier and idle time
// for a row that discovery found intact.
func inspect(ctx context.Context, w *Worktree, e env) {
	if w.State == StateError {
		return
	}
	if w.State == StateOK && !exists(w.Path) {
		w.State = StateMissing
		w.Hint = "dir vanished during the scan"
	}
	if w.State == StateMissing || w.State == StateBroken || w.State == StateMoved {
		w.LastCommit = lastCommit(ctx, w.Repo.CommonDir, w.Head)
		w.setIdle(e.now)
		return
	}
	embedded, err := inspectGit(ctx, w)
	if err != nil {
		if w.State == StateOK {
			w.State = StateError
			w.Hint = err.Error()
		}
		return
	}
	w.InUse = processesWithin(e.procs, w.ID)
	w.Nested = nestedWithin(w, e.registered)
	if w.State == StateOK {
		switch {
		case anyWithin(e.cwds, w.ID):
			w.State = StateCurrent
		case exists(filepath.Join(w.AdminDir, "modules")):
			w.State = StateSubmodules
			w.Hint = "has initialized submodules (--force would delete their gitdirs): remove manually"
		case len(embedded) > 0:
			w.State = StateSubmodules
			w.Hint = "has submodule checkouts with their own .git (removal would delete them): remove manually: " + strings.Join(embedded, ", ")
		case len(w.Nested) > 0:
			w.State = StateNested
			w.Hint = "remove the inner one first: " + strings.Join(w.Nested, ", ")
		}
	}
	if err := classifyBranch(ctx, w); err != nil {
		w.Branch.Tier = TierNone
		w.Branch.KeepReason = "error: " + err.Error()
	}
	w.setIdle(e.now)
}

// inspectGit reads status, the index and reflogs. It returns the gitlinks
// whose dir holds a .git entry (embedded repos or submodule checkouts).
func inspectGit(ctx context.Context, w *Worktree) (embedded []string, err error) {
	out, err := git.Run(ctx, w.Path, "status", "--porcelain=v2", "--branch", "-z",
		"-unormal", "--ignore-submodules=none", "--ignored=matching")
	if err != nil {
		return nil, err
	}
	st, err := git.ParseStatus(out)
	if err != nil {
		return nil, err
	}
	w.Head = st.OID
	if st.OID == "(initial)" {
		w.Head = ""
	}
	b := &w.Branch
	b.Upstream, b.Ahead, b.Behind = st.Upstream, st.Ahead, st.Behind
	b.UpstreamGone = st.Upstream != "" && !st.HasAB
	for _, entry := range st.Entries {
		switch entry.Kind {
		case git.KindChanged, git.KindRenamed:
			if entry.XY[0] != '.' {
				w.Staged++
			}
			if entry.XY[1] != '.' {
				w.Modified++
			}
			w.Changes = append(w.Changes, Change{Code: strings.ReplaceAll(entry.XY, ".", ""), Path: entry.Path, OrigPath: entry.OrigPath})
		case git.KindConflicted:
			w.Conflicted++
			w.Changes = append(w.Changes, Change{Code: entry.XY, Path: entry.Path})
		case git.KindUntracked:
			w.Untracked++
			w.Changes = append(w.Changes, Change{Code: "??", Path: entry.Path})
		case git.KindIgnored:
			w.addIgnored(entry.Path)
		}
	}

	lsFiles, err := git.Run(ctx, w.Path, "ls-files", "-v", "-s", "-z")
	if err != nil {
		return nil, err
	}
	index := git.ParseIndex(lsFiles)
	w.Hidden = hiddenChanges(ctx, w.Path, index)
	for _, e := range index {
		if e.Gitlink() && exists(filepath.Join(w.Path, e.Path, ".git")) {
			embedded = append(embedded, e.Path)
		}
	}
	w.InProgress = inProgress(w.AdminDir)

	if w.Detached && w.Head != "" {
		if w.DetachedLost, err = git.CountUnreachable(ctx, w.Path, w.Head, nil); err != nil {
			return nil, err
		}
	}
	if w.ReflogOnly, err = reflogOnly(ctx, w); err != nil {
		return nil, err
	}
	w.LastCommit = lastCommit(ctx, w.Path, w.Head)
	return embedded, nil
}

func (w *Worktree) addIgnored(path string) {
	name := filepath.Base(strings.TrimSuffix(path, "/"))
	e := IgnoredEntry{Path: path, Dir: strings.HasSuffix(path, "/")}
	if e.Dir {
		e.Regenerable = regenerableDirs[name]
	} else {
		e.Regenerable = name == ".DS_Store"
	}
	w.Ignored = append(w.Ignored, e)
	switch {
	case e.Regenerable:
	case e.Dir:
		w.IgnoredDirs = append(w.IgnoredDirs, path)
	default:
		w.IgnoredFiles = append(w.IgnoredFiles, path)
	}
}

// hiddenChanges finds edits status can't see: assume-unchanged files, and
// skip-worktree files that exist on disk with content differing from the
// index. Absent skip-worktree files are just sparse checkout.
func hiddenChanges(ctx context.Context, dir string, index []git.IndexEntry) []string {
	var hidden, toHash, wantSHA []string
	for _, e := range index {
		switch {
		case e.SkipWorktree():
			if e.Gitlink() {
				continue
			}
			info, err := os.Lstat(filepath.Join(dir, e.Path))
			switch {
			case os.IsNotExist(err):
				continue
			case err != nil, info.IsDir(), strings.Contains(e.Path, "\n"):
				hidden = append(hidden, e.Path)
			case info.Mode()&os.ModeSymlink != 0 || e.Mode == "120000":
				if differs, err := symlinkDiffers(ctx, dir, e, info); err != nil || differs {
					hidden = append(hidden, e.Path)
				}
			default:
				toHash = append(toHash, e.Path)
				wantSHA = append(wantSHA, e.SHA)
			}
		case e.AssumeUnchanged():
			hidden = append(hidden, e.Path)
		}
	}
	if len(toHash) == 0 {
		return hidden
	}
	out, err := git.RunInput(ctx, dir, strings.Join(toHash, "\n")+"\n", "hash-object", "--stdin-paths")
	got := strings.Fields(out)
	for i, p := range toHash {
		// Can't tell (eg. a clean filter failed): assume it was edited.
		if err != nil || len(got) != len(toHash) || got[i] != wantSHA[i] {
			hidden = append(hidden, p)
		}
	}
	slices.Sort(hidden)
	return hidden
}

func symlinkDiffers(ctx context.Context, dir string, e git.IndexEntry, info os.FileInfo) (bool, error) {
	if info.Mode()&os.ModeSymlink == 0 || e.Mode != "120000" {
		return true, nil
	}
	target, err := os.Readlink(filepath.Join(dir, e.Path))
	if err != nil {
		return true, err
	}
	sha, err := git.RunInput(ctx, dir, target, "hash-object", "--stdin")
	return strings.TrimSpace(sha) != e.SHA, err
}

// reflogOnly lists commits reachable from the worktree's HEAD reflog or its
// branch's reflog that no branch, remote or tag contains (eg. after a reset
// or detaching away). The current HEAD is excluded: DetachedLost covers it.
// Removing the worktree deletes the HEAD reflog; deleting the branch
// deletes the other.
func reflogOnly(ctx context.Context, w *Worktree) ([]Commit, error) {
	refs := []string{"HEAD"}
	if w.Branch.Name != "" {
		refs = append(refs, "refs/heads/"+w.Branch.Name)
	}
	var tips []string
	for _, ref := range refs {
		// A missing or unreadable reflog only hides a note.
		out, _ := git.Run(ctx, w.Path, "reflog", "show", "--format=%H", "-n", strconv.Itoa(reflogDepth), ref, "--")
		tips = append(tips, strings.Fields(out)...)
	}
	slices.Sort(tips)
	tips = slices.Compact(tips)
	if len(tips) == 0 {
		return nil, nil
	}
	args := append([]string{"log", "--format=%H%x00%s", "--ignore-missing"}, tips...)
	args = append(args, "--not")
	if w.Head != "" {
		args = append(args, w.Head)
	}
	// Excluding every ref is slow in repos with thousands of them; most
	// reflogs only hold ancestors of HEAD or a default branch, so try those
	// first.
	out, err := git.Run(ctx, w.Path, slices.Concat(args, w.Repo.defaultBranches, []string{"--"})...)
	if err != nil || strings.TrimSpace(out) == "" {
		return nil, err
	}
	out, err = git.Run(ctx, w.Path, slices.Concat(args, []string{"--branches", "--remotes", "--tags", "--"})...)
	if err != nil {
		return nil, err
	}
	var commits []Commit
	for _, line := range strings.Split(strings.TrimSuffix(out, "\n"), "\n") {
		if sha, subject, ok := strings.Cut(line, "\x00"); ok {
			commits = append(commits, Commit{SHA: sha, Subject: subject})
		}
	}
	return commits, nil
}

// opHeads are files in an admin dir that name commits an in-progress
// operation would need (eg. the pre-rebase tip).
var opHeads = []string{"ORIG_HEAD", "MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD", "rebase-merge/orig-head", "rebase-apply/orig-head"}

// LostCommits lists every commit that removing the worktree would leave
// unreachable from any branch, remote or tag: its HEAD (when detached) and
// any in-progress operation's heads. Reflog-only commits are in ReflogOnly.
func LostCommits(ctx context.Context, w *Worktree) ([]string, error) {
	var tips []string
	if w.Head != "" {
		tips = append(tips, w.Head)
	}
	for _, name := range opHeads {
		data, err := os.ReadFile(filepath.Join(w.AdminDir, name))
		if err == nil {
			tips = append(tips, strings.Fields(string(data))...)
		}
	}
	if len(tips) == 0 {
		return nil, nil
	}
	args := append([]string{"rev-list", "--ignore-missing"}, tips...)
	out, err := git.Run(ctx, w.Repo.CommonDir, append(args, "--not", "--branches", "--remotes", "--tags", "--")...)
	if err != nil {
		return nil, err
	}
	return strings.Fields(out), nil
}

func inProgress(adminDir string) string {
	for _, op := range []struct{ file, label string }{
		{"rebase-merge", "REBASING"},
		{"rebase-apply", "REBASING"},
		{"MERGE_HEAD", "MERGING"},
		{"CHERRY_PICK_HEAD", "CHERRY-PICKING"},
		{"REVERT_HEAD", "REVERTING"},
		{"BISECT_LOG", "BISECTING"},
	} {
		if exists(filepath.Join(adminDir, op.file)) {
			return op.label
		}
	}
	return ""
}

func lastCommit(ctx context.Context, dir, sha string) Commit {
	if sha == "" {
		return Commit{}
	}
	out, err := git.Run(ctx, dir, "log", "-1", "--format=%H%x00%ct%x00%s", sha, "--")
	if err != nil {
		return Commit{}
	}
	parts := strings.SplitN(strings.TrimSuffix(out, "\n"), "\x00", 3)
	if len(parts) != 3 {
		return Commit{}
	}
	secs, _ := strconv.ParseInt(parts[1], 10, 64)
	return Commit{SHA: parts[0], Time: time.Unix(secs, 0), Subject: parts[2]}
}

// setIdle uses the newest of the HEAD commit, the worktree's HEAD reflog,
// and every changed path: editing files bumps nothing in git. The index is
// skipped: any tool running a plain `git status` rewrites it.
func (w *Worktree) setIdle(now time.Time) {
	last := w.LastCommit.Time
	newer := func(p string) {
		if info, err := os.Lstat(p); err == nil && info.ModTime().After(last) {
			last = info.ModTime()
		}
	}
	if w.AdminDir != "" {
		newer(filepath.Join(w.AdminDir, "logs", "HEAD"))
	}
	for _, c := range w.Changes {
		newer(filepath.Join(w.Path, strings.TrimSuffix(c.Path, "/")))
	}
	w.LastActive = last
	if !last.IsZero() {
		w.Idle = max(now.Sub(last), 0)
	}
}

func processesWithin(procs []Process, dir string) []Process {
	var in []Process
	for _, p := range procs {
		if within(p.Cwd, dir) {
			in = append(in, p)
		}
	}
	return in
}

func anyWithin(paths []string, dir string) bool {
	for _, p := range paths {
		if within(p, dir) {
			return true
		}
	}
	return false
}

// nestedWithin finds registered worktrees strictly inside w, plus any .git
// under its untracked/ignored dirs (allowlisted ones too): git's removal
// recurses into those even without --force.
func nestedWithin(w *Worktree, registered []string) []string {
	var nested []string
	for _, p := range registered {
		if strictlyWithin(p, w.ID) {
			nested = append(nested, p)
		}
	}
	var dirs []string
	for _, c := range w.Changes {
		if c.Code == "??" && strings.HasSuffix(c.Path, "/") {
			dirs = append(dirs, c.Path)
		}
	}
	for _, e := range w.Ignored {
		if e.Dir {
			dirs = append(dirs, e.Path)
		}
	}
	for _, d := range dirs {
		for _, found := range findGitEntries(filepath.Join(w.ID, d), 3) {
			if !slices.ContainsFunc(nested, func(q string) bool { return pathKey(q) == pathKey(found) }) {
				nested = append(nested, found)
			}
		}
	}
	return nested
}

// findGitEntries returns dirs at most depth levels below dir (dir included)
// that contain a .git entry, skipping node_modules below dir.
func findGitEntries(dir string, depth int) []string {
	if exists(filepath.Join(dir, ".git")) {
		return []string{Canonical(dir)}
	}
	if depth == 0 {
		return nil
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var found []string
	for _, e := range entries {
		if e.IsDir() && e.Name() != "node_modules" {
			found = append(found, findGitEntries(filepath.Join(dir, e.Name()), depth-1)...)
		}
	}
	return found
}
