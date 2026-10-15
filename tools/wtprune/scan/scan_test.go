package scan_test

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"wtprune/scan"
)

func TestScan_CleanWorktreeIsSafe(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")

	res := scanRepo(t, repo)

	if got := names(res); !reflect.DeepEqual(got, []string{"wt"}) {
		t.Fatalf("rows = %v, want only the linked worktree", got)
	}
	w := row(t, res, "wt")
	if !w.Safe() || w.StatusText() != "clean" {
		t.Errorf("state %q status %q, want a safe clean row", w.State, w.StatusText())
	}
	if w.ID != canonical(t, wt) {
		t.Errorf("ID = %q, want canonical %q", w.ID, canonical(t, wt))
	}
	if w.Repo.Name != "repo" || w.Repo.CommonDir != canonical(t, filepath.Join(repo, ".git")) {
		t.Errorf("repo = %q %q", w.Repo.Name, w.Repo.CommonDir)
	}
}

func TestScan_RepoModeFromAnyWorktree(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a"), "-b", "a")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "b"), "-b", "b")

	res := scanRepo(t, filepath.Join(root, "a"))

	if got := names(res); len(got) != 2 {
		t.Errorf("rows = %v, want a and b", got)
	}
}

func TestScan_RepoModeOutsideRepoErrors(t *testing.T) {
	isolateGit(t)

	_, err := scan.Scan(context.Background(), scan.Options{Repo: t.TempDir(), Processes: noProcesses})

	if err == nil || !strings.Contains(err.Error(), "--dir") {
		t.Errorf("err = %v, want a hint to use --dir", err)
	}
}

func TestScan_CurrentViaSymlinkedCwd(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "sub", "f"), "x\n")
	run(t, wt, "git", "add", ".")
	run(t, wt, "git", "commit", "-qm", "sub")

	// root is under /var/folders, a symlink to /private/var/folders on macOS.
	res, err := scan.Scan(context.Background(), scan.Options{Repo: repo, Cwd: filepath.Join(wt, "sub"), Processes: noProcesses})
	if err != nil {
		t.Fatal(err)
	}

	if w := row(t, res, "wt"); w.State != scan.StateCurrent {
		t.Errorf("state = %q, want current", w.State)
	}
}

func TestScan_Locked(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, repo, "git", "worktree", "lock", "--reason", "on usb", wt)

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateLocked || w.LockReason != "on usb" || !strings.Contains(w.Hint, "git worktree unlock") {
		t.Errorf("state %q reason %q hint %q", w.State, w.LockReason, w.Hint)
	}
}

func TestScan_LockedAndMissingStaysLocked(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, repo, "git", "worktree", "lock", wt)
	if err := os.RemoveAll(wt); err != nil {
		t.Fatal(err)
	}

	if w := row(t, scanRepo(t, repo), "wt"); w.State != scan.StateLocked {
		t.Errorf("state = %q hint %q, want locked", w.State, w.Hint)
	}
}

func TestScan_MissingInRepoMode(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	if err := os.RemoveAll(wt); err != nil {
		t.Fatal(err)
	}

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateMissing || w.Selectable() {
		t.Errorf("state = %q, want blocked missing", w.State)
	}
}

func TestScan_MovedIsMissingInRepoModeAndMovedInDirMode(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wts", "old"), "-b", "feat")
	if err := os.Rename(filepath.Join(root, "wts", "old"), filepath.Join(root, "wts", "new")); err != nil {
		t.Fatal(err)
	}

	if w := row(t, scanRepo(t, repo), "old"); w.State != scan.StateMissing {
		t.Errorf("repo mode: state = %q, want missing", w.State)
	}
	w := row(t, scanDir(t, filepath.Join(root, "wts"), 3), "new")
	if w.State != scan.StateMoved || !strings.Contains(w.Hint, "repair") {
		t.Errorf("dir mode: state %q hint %q, want moved with a repair hint", w.State, w.Hint)
	}
}

func TestScan_BrokenWhenAdminDirGone(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wts", "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	if err := os.RemoveAll(filepath.Join(repo, ".git", "worktrees", "wt")); err != nil {
		t.Fatal(err)
	}

	w := row(t, scanDir(t, filepath.Join(root, "wts"), 3), "wt")

	if w.State != scan.StateBroken {
		t.Errorf("state = %q, want broken", w.State)
	}
}

func TestScan_Submodules(t *testing.T) {
	root, repo := newRepo(t)
	sub := filepath.Join(root, "sub")
	run(t, root, "git", "init", "-q", "-b", "main", sub)
	run(t, sub, "git", "commit", "-q", "--allow-empty", "-m", "s")
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "-c", "protocol.file.allow=always", "submodule", "add", "-q", sub, "sm")
	run(t, wt, "git", "commit", "-qm", "add submodule")

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateSubmodules {
		t.Errorf("state = %q, want submodules", w.State)
	}
}

func TestScan_NestedWorktreeInIgnoredDir(t *testing.T) {
	root, repo := newRepo(t)
	outer := filepath.Join(root, "outer")
	run(t, repo, "git", "worktree", "add", "-q", outer, "-b", "outer")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(outer, "build", "inner"), "-b", "inner")

	res := scanRepo(t, repo)

	w := row(t, res, "outer")
	if w.State != scan.StateNested || !reflect.DeepEqual(w.Nested, []string{canonical(t, filepath.Join(outer, "build", "inner"))}) {
		t.Errorf("state %q nested %v, want nested containing inner", w.State, w.Nested)
	}
	if inner := row(t, res, "inner"); !inner.Selectable() {
		t.Errorf("inner state = %q, want selectable", inner.State)
	}
}

func TestScan_NestedRepoFoundByWalk(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "init", "-q", filepath.Join(wt, "scratch", "a", "b", "inner"))

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateNested || !reflect.DeepEqual(w.Nested, []string{canonical(t, filepath.Join(wt, "scratch", "a", "b", "inner"))}) {
		t.Errorf("state %q nested %v, want the inner repo", w.State, w.Nested)
	}
}

func TestScan_DirtyCounts(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "staged"), "s\n")
	run(t, wt, "git", "add", "staged")
	write(t, filepath.Join(wt, "README"), "edited\n")
	write(t, filepath.Join(wt, "new1"), "n\n")
	write(t, filepath.Join(wt, "newdir", "f"), "n\n")

	w := row(t, scanRepo(t, repo), "wt")

	if !w.AtRisk() || w.StatusText() != "DIRTY 1S 1M 2?" {
		t.Errorf("status = %q, want DIRTY 1S 1M 2?", w.StatusText())
	}
	want := []scan.Change{{Code: "M", Path: "README"}, {Code: "A", Path: "staged"}, {Code: "??", Path: "new1"}, {Code: "??", Path: "newdir/"}}
	if !reflect.DeepEqual(w.Changes, want) {
		t.Errorf("changes = %+v\nwant %+v", w.Changes, want)
	}
}

func TestScan_UntrackedSeenDespiteRepoConfigHidingThem(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, repo, "git", "config", "status.showUntrackedFiles", "no")
	write(t, filepath.Join(wt, "notes.md"), "n\n")

	w := row(t, scanRepo(t, repo), "wt")

	if w.Untracked != 1 || !w.AtRisk() {
		t.Errorf("untracked = %d at risk = %v, want 1 and at risk", w.Untracked, w.AtRisk())
	}
}

func TestScan_ConflictedMerge(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "README"), "feat\n")
	run(t, wt, "git", "commit", "-qam", "feat")
	write(t, filepath.Join(repo, "README"), "main\n")
	run(t, repo, "git", "commit", "-qam", "main")
	tryRun(t, wt, "git", "merge", "main")

	w := row(t, scanRepo(t, repo), "wt")

	if w.Conflicted != 1 || w.InProgress != "MERGING" || w.StatusText() != "DIRTY 1U MERGING" {
		t.Errorf("status = %q, want DIRTY 1U MERGING", w.StatusText())
	}
	if w.Branch.Candidate() || w.Branch.KeepReason != "in progress" {
		t.Errorf("branch = %+v, want kept while in progress", w.Branch)
	}
}

func TestScan_InProgressOps(t *testing.T) {
	root, repo := newRepo(t)
	write(t, filepath.Join(repo, "README"), "main\n")
	run(t, repo, "git", "commit", "-qam", "main")
	mainTip := run(t, repo, "git", "rev-parse", "HEAD")

	for _, tc := range []struct {
		name string
		op   []string
		want string
	}{
		{"rebasing", []string{"rebase", "main"}, "REBASING"},
		{"picking", []string{"cherry-pick", mainTip}, "CHERRY-PICKING"},
		{"bisecting", []string{"bisect", "start"}, "BISECTING"},
		{"reverting", []string{"revert", "--no-commit", "HEAD"}, "REVERTING"},
	} {
		wt := filepath.Join(root, tc.name)
		run(t, repo, "git", "worktree", "add", "-q", wt, "-b", tc.name, "HEAD~1")
		write(t, filepath.Join(wt, "README"), tc.name+"\n")
		run(t, wt, "git", "commit", "-qam", tc.name)
		tryRun(t, wt, "git", tc.op...)
	}
	res := scanRepo(t, repo)

	for _, want := range []struct{ name, op string }{
		{"rebasing", "REBASING"}, {"picking", "CHERRY-PICKING"}, {"bisecting", "BISECTING"}, {"reverting", "REVERTING"},
	} {
		if w := row(t, res, want.name); w.InProgress != want.op || !w.AtRisk() {
			t.Errorf("%s: in progress = %q at risk = %v, want %s", want.name, w.InProgress, w.AtRisk(), want.op)
		}
	}
}

func TestScan_DetachedLostCommits(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", wt)
	write(t, filepath.Join(wt, "f"), "1\n")
	run(t, wt, "git", "add", "f")
	run(t, wt, "git", "commit", "-qm", "lost 1")
	write(t, filepath.Join(wt, "f"), "2\n")
	run(t, wt, "git", "commit", "-qam", "lost 2")

	w := row(t, scanRepo(t, repo), "wt")

	if w.DetachedLost != 2 || w.StatusText() != "DETACHED +2 LOST" {
		t.Errorf("status = %q, want DETACHED +2 LOST", w.StatusText())
	}
	if w.Branch.KeepReason != "detached" {
		t.Errorf("branch keep reason = %q, want detached", w.Branch.KeepReason)
	}
}

func TestScan_DetachedAtExistingCommitIsSafe(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", "--detach", filepath.Join(root, "wt"))

	if w := row(t, scanRepo(t, repo), "wt"); !w.Safe() {
		t.Errorf("status = %q, want safe", w.StatusText())
	}
}

func TestScan_StashDoesNotAnchorDetachedCommits(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", wt)
	write(t, filepath.Join(wt, "README"), "lost\n")
	run(t, wt, "git", "commit", "-qam", "lost")
	write(t, filepath.Join(wt, "README"), "stashed\n")
	run(t, wt, "git", "stash", "-q")

	w := row(t, scanRepo(t, repo), "wt")

	// The stash contains HEAD, but refs/stash is not a branch/remote/tag;
	// and a stash is not a working-tree change.
	if w.DetachedLost != 1 || w.Dirty() {
		t.Errorf("lost = %d dirty = %v, want 1 lost and not dirty", w.DetachedLost, w.Dirty())
	}
}

func TestScan_HiddenAssumeUnchanged(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "update-index", "--assume-unchanged", "README")
	write(t, filepath.Join(wt, "README"), "invisible edit\n")

	w := row(t, scanRepo(t, repo), "wt")

	if w.Dirty() || !reflect.DeepEqual(w.Hidden, []string{"README"}) || w.StatusText() != "HIDDEN 1" {
		t.Errorf("status %q hidden %v, want HIDDEN 1 and not dirty", w.StatusText(), w.Hidden)
	}
}

func TestScan_InUseViaProcessCwd(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "sub", "f"), "x\n")
	run(t, wt, "git", "add", ".")
	run(t, wt, "git", "commit", "-qm", "sub")
	procs := func(context.Context) ([]scan.Process, error) {
		return []scan.Process{
			{PID: 101, Command: "zsh", Cwd: filepath.Join(wt, "sub")}, // non-canonical /var path
			{PID: 102, Command: "claude", Cwd: canonical(t, wt)},
			{PID: 103, Command: "vim", Cwd: canonical(t, wt) + "-other"},
		}, nil
	}

	res, err := scan.Scan(context.Background(), scan.Options{Repo: repo, Cwd: "/", Processes: procs})
	if err != nil {
		t.Fatal(err)
	}

	if w := row(t, res, "wt"); w.StatusText() != "IN USE zsh,claude" {
		t.Errorf("status = %q, want IN USE zsh,claude", w.StatusText())
	}
}

func TestScan_IgnoredFilesNoteWithoutRisk(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, ".env"), "SECRET=1\n")
	write(t, filepath.Join(wt, ".DS_Store"), "x")
	write(t, filepath.Join(wt, "build", "out.bin"), "x")

	w := row(t, scanRepo(t, repo), "wt")

	if !w.Safe() || !w.NeedsTrash() || w.StatusText() != "clean +1 ignored" {
		t.Errorf("status %q safe %v needs trash %v, want safe 'clean +1 ignored' going via Trash", w.StatusText(), w.Safe(), w.NeedsTrash())
	}
	wantIgnored := []scan.IgnoredEntry{{Path: ".DS_Store", Regenerable: true}, {Path: ".env"}, {Path: "build/", Dir: true, Regenerable: true}}
	if !reflect.DeepEqual(w.IgnoredFiles, []string{".env"}) || !reflect.DeepEqual(w.Ignored, wantIgnored) {
		t.Errorf("ignored files %v, ignored %+v", w.IgnoredFiles, w.Ignored)
	}
}

func TestScan_IdleUsesNewestSignal(t *testing.T) {
	root, repo := newRepo(t)
	t.Setenv("GIT_COMMITTER_DATE", "2020-01-01T00:00:00Z")
	for _, name := range []string{"old", "edited", "checkedout"} {
		wt := filepath.Join(root, name)
		run(t, repo, "git", "worktree", "add", "-q", wt, "-b", name)
		run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "old")
	}
	longAgo := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
	now := time.Now()
	for _, name := range []string{"old", "edited", "checkedout"} {
		admin := filepath.Join(repo, ".git", "worktrees", name)
		// Any plain `git status` rewrites the index: it must not count.
		if err := os.Chtimes(filepath.Join(admin, "index"), now, now); err != nil {
			t.Fatal(err)
		}
		if name != "checkedout" {
			if err := os.Chtimes(filepath.Join(admin, "logs", "HEAD"), longAgo, longAgo); err != nil {
				t.Fatal(err)
			}
		}
	}
	write(t, filepath.Join(root, "edited", "README"), "edited just now\n")

	res, err := scan.Scan(context.Background(), scan.Options{Repo: repo, Cwd: "/", Processes: noProcesses, Now: func() time.Time { return now }})
	if err != nil {
		t.Fatal(err)
	}

	if got := names(res); got[0] != "old" {
		t.Errorf("order = %v, want oldest idle (old) first", got)
	}
	if w := row(t, res, "old"); !w.LastActive.Equal(longAgo) {
		t.Errorf("old last active = %v, want %v (index mtime ignored)", w.LastActive, longAgo)
	}
	if w := row(t, res, "edited"); w.Idle > time.Minute {
		t.Errorf("edited idle = %v, want under a minute (file edit counts)", w.Idle)
	}
	if w := row(t, res, "checkedout"); w.Idle > time.Minute {
		t.Errorf("checkedout idle = %v, want under a minute (logs/HEAD counts)", w.Idle)
	}
}

func TestFormatIdle(t *testing.T) {
	for _, tc := range []struct {
		d    time.Duration
		want string
	}{
		{0, "0m"},
		{59 * time.Minute, "59m"},
		{time.Hour, "1h"},
		{23*time.Hour + 59*time.Minute, "23h"},
		{24 * time.Hour, "1d"},
		{400 * 24 * time.Hour, "400d"},
	} {
		if got := scan.FormatIdle(tc.d); got != tc.want {
			t.Errorf("FormatIdle(%v) = %q, want %q", tc.d, got, tc.want)
		}
	}
}

func TestScan_ProgressReportsEveryRow(t *testing.T) {
	root, repo := newRepo(t)
	for _, name := range []string{"a", "b", "c"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), "-b", name)
	}
	var calls [][2]int
	progress := func(done, total int) { calls = append(calls, [2]int{done, total}) }

	_, err := scan.Scan(context.Background(), scan.Options{Repo: repo, Cwd: "/", Processes: noProcesses, Progress: progress})
	if err != nil {
		t.Fatal(err)
	}

	if want := [][2]int{{1, 3}, {2, 3}, {3, 3}}; !reflect.DeepEqual(calls, want) {
		t.Errorf("progress calls = %v, want %v", calls, want)
	}
}

func TestScan_ScanDoesNotTouchIndex(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	// A stale-stat index entry makes plain `git status` rewrite the index.
	longAgo := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
	index := filepath.Join(repo, ".git", "worktrees", "wt", "index")
	if err := os.Chtimes(filepath.Join(wt, "README"), longAgo, longAgo); err != nil {
		t.Fatal(err)
	}
	if err := os.Chtimes(index, longAgo, longAgo); err != nil {
		t.Fatal(err)
	}

	scanRepo(t, repo)

	info, err := os.Stat(index)
	if err != nil {
		t.Fatal(err)
	}
	if !info.ModTime().Equal(longAgo) {
		t.Errorf("index mtime = %v, want untouched %v", info.ModTime(), longAgo)
	}
}
