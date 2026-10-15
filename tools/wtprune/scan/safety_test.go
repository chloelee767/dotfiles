package scan_test

import (
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"testing"

	"wtprune/scan"
)

func TestScan_SkipWorktreeEditedIsHidden(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "update-index", "--skip-worktree", "README")
	write(t, filepath.Join(wt, "README"), "invisible edit\n")

	w := row(t, scanRepo(t, repo), "wt")

	if w.Dirty() || !reflect.DeepEqual(w.Hidden, []string{"README"}) || w.StatusText() != "HIDDEN 1" {
		t.Errorf("status %q hidden %v, want HIDDEN 1 and not dirty", w.StatusText(), w.Hidden)
	}
}

func TestScan_SkipWorktreeUneditedIsNotHidden(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "update-index", "--skip-worktree", "README")

	w := row(t, scanRepo(t, repo), "wt")

	if len(w.Hidden) != 0 || !w.Safe() {
		t.Errorf("status %q hidden %v, want a safe row", w.StatusText(), w.Hidden)
	}
}

func TestScan_SparseCheckoutAbsentFilesAreNotHidden(t *testing.T) {
	root, repo := newRepo(t)
	write(t, filepath.Join(repo, "keep", "k"), "k\n")
	write(t, filepath.Join(repo, "drop", "d"), "d\n")
	run(t, repo, "git", "add", ".")
	run(t, repo, "git", "commit", "-qm", "dirs")
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "sparse-checkout", "set", "keep")
	if _, err := os.Lstat(filepath.Join(wt, "drop", "d")); !os.IsNotExist(err) {
		t.Fatalf("setup: drop/d should be absent, got %v", err)
	}

	w := row(t, scanRepo(t, repo), "wt")

	if len(w.Hidden) != 0 || !w.Safe() {
		t.Errorf("status %q hidden %v, want a safe row", w.StatusText(), w.Hidden)
	}
}

func TestScan_HiddenPathsAreInFingerprint(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "update-index", "--skip-worktree", "README")
	before := row(t, scanRepo(t, repo), "wt").Fingerprint()
	write(t, filepath.Join(wt, "README"), "invisible edit\n")

	after := row(t, scanRepo(t, repo), "wt").Fingerprint()

	if before == after || !strings.Contains(after, "hidden=README") {
		t.Errorf("fingerprint did not change with a hidden edit:\n%s", after)
	}
}

func TestScan_IgnoredDirsOnAllowlistArePlainRemoval(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "build", "out.bin"), "x")

	w := row(t, scanRepo(t, repo), "wt")

	if !w.Safe() || w.NeedsTrash() || w.StatusText() != "clean" || len(w.IgnoredDirs) != 0 {
		t.Errorf("status %q needs trash %v ignored dirs %v, want plain clean", w.StatusText(), w.NeedsTrash(), w.IgnoredDirs)
	}
	if want := []scan.IgnoredEntry{{Path: "build/", Dir: true, Regenerable: true}}; !reflect.DeepEqual(w.Ignored, want) {
		t.Errorf("ignored = %+v, want %+v", w.Ignored, want)
	}
}

func TestScan_IgnoredDirsOffAllowlistGoViaTrash(t *testing.T) {
	root, repo := newRepo(t)
	write(t, filepath.Join(repo, ".git", "info", "exclude"), "data/\n")
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "data", "fixtures.db"), "precious")
	write(t, filepath.Join(wt, "build", "out.bin"), "x")

	w := row(t, scanRepo(t, repo), "wt")

	if !w.Safe() || !w.NeedsTrash() || w.StatusText() != "clean +1 ignored" {
		t.Errorf("status %q safe %v needs trash %v, want safe 'clean +1 ignored' via Trash", w.StatusText(), w.Safe(), w.NeedsTrash())
	}
	if !reflect.DeepEqual(w.IgnoredDirs, []string{"data/"}) || !strings.Contains(w.Fingerprint(), "ignored=data/") {
		t.Errorf("ignored dirs %v, fingerprint:\n%s", w.IgnoredDirs, w.Fingerprint())
	}
}

func TestScan_NestedRepoInsideAllowlistedIgnoredDir(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "init", "-q", filepath.Join(wt, "build", "a", "inner"))

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateNested {
		t.Errorf("state %q, want nested", w.State)
	}
}

func TestScan_ReflogOnlyAfterResetHard(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "dropped by reset")
	dropped := run(t, wt, "git", "rev-parse", "HEAD")
	run(t, wt, "git", "reset", "-q", "--hard", "HEAD~1")

	w := row(t, scanRepo(t, repo), "wt")

	if want := []scan.Commit{{SHA: dropped, Subject: "dropped by reset"}}; !reflect.DeepEqual(w.ReflogOnly, want) {
		t.Errorf("reflog only = %+v, want %+v", w.ReflogOnly, want)
	}
	if !w.Safe() {
		t.Errorf("status %q, want reflog-only commits to be a note, not a risk", w.StatusText())
	}
}

func TestScan_ReflogOnlyAfterDetachingAway(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "checkout", "-q", "--detach")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "left behind")
	left := run(t, wt, "git", "rev-parse", "HEAD")
	run(t, wt, "git", "checkout", "-q", "feat")

	w := row(t, scanRepo(t, repo), "wt")

	if want := []scan.Commit{{SHA: left, Subject: "left behind"}}; !reflect.DeepEqual(w.ReflogOnly, want) {
		t.Errorf("reflog only = %+v, want %+v", w.ReflogOnly, want)
	}
}

func TestScan_DetachedLostIsNotAlsoReflogOnly(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", wt)
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "lost")

	w := row(t, scanRepo(t, repo), "wt")

	if w.DetachedLost != 1 || len(w.ReflogOnly) != 0 {
		t.Errorf("lost %d reflog only %+v, want 1 lost counted once", w.DetachedLost, w.ReflogOnly)
	}
}

func TestScan_EmbeddedRepoGitlinkIsBlocked(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	inner := filepath.Join(wt, "inner")
	run(t, wt, "git", "init", "-q", "-b", "main", inner)
	run(t, inner, "git", "commit", "-q", "--allow-empty", "-m", "unpushed inner work")
	run(t, wt, "git", "add", "inner")
	run(t, wt, "git", "commit", "-qm", "embed")

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateSubmodules {
		t.Errorf("state %q status %q, want submodules", w.State, w.StatusText())
	}
}

func TestScan_UninitializedGitlinkIsNotBlocked(t *testing.T) {
	root, repo := newRepo(t)
	sub := filepath.Join(root, "sub")
	run(t, root, "git", "init", "-q", "-b", "main", sub)
	run(t, sub, "git", "commit", "-q", "--allow-empty", "-m", "s")
	run(t, repo, "git", "-c", "protocol.file.allow=always", "submodule", "add", "-q", sub, "sm")
	run(t, repo, "git", "commit", "-qm", "add submodule")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")

	w := row(t, scanRepo(t, repo), "wt")

	if w.State != scan.StateOK {
		t.Errorf("state %q, want selectable (submodule never initialized here)", w.State)
	}
}

func TestScan_BrokenRowOfBareRepoNamedAfterRepo(t *testing.T) {
	isolateGit(t)
	root := t.TempDir()
	bare := filepath.Join(root, "proj.git")
	run(t, root, "git", "init", "-q", "--bare", "-b", "main", bare)
	run(t, root, "git", "clone", "-q", bare, filepath.Join(root, "seed"))
	run(t, filepath.Join(root, "seed"), "git", "commit", "-q", "--allow-empty", "-m", "init")
	run(t, filepath.Join(root, "seed"), "git", "push", "-q", "origin", "main")
	wt := filepath.Join(root, "wts", "wt")
	run(t, bare, "git", "worktree", "add", "-q", wt, "-b", "feat")
	if err := os.RemoveAll(filepath.Join(bare, "worktrees", "wt")); err != nil {
		t.Fatal(err)
	}

	w := row(t, scanDir(t, filepath.Join(root, "wts"), 3), "wt")

	if w.State != scan.StateBroken || w.Repo.Name != "proj" {
		t.Errorf("state %q repo name %q, want broken row of proj", w.State, w.Repo.Name)
	}
}

// On a case-insensitive volume, git may have registered the path with a
// different case than the dir on disk: fall back to a case-folded match.
func TestScan_CaseFoldedRegistrationFallback(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("case-folded matching only applies on macOS")
	}
	root, repo := newRepo(t)
	wts := filepath.Join(root, "wts")
	wt := filepath.Join(wts, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(repo, ".git", "worktrees", "wt", "gitdir"), filepath.Join(canonical(t, wts), "WT", ".git")+"\n")

	res := scanDir(t, wts, 3)

	if len(res.Rows) != 1 || res.Rows[0].State != scan.StateOK {
		t.Fatalf("rows %v, want one selectable row", names(res))
	}
	if w := res.Rows[0]; w.Hint != "" {
		t.Errorf("state %q hint %q, want a selectable row", w.State, w.Hint)
	}
}

func TestParseLsof(t *testing.T) {
	out := "p101\nczsh\nfcwd\nn/Users/me/wt\np202\ncclaude code\nfcwd\nn/tmp/a b\n"

	got := scan.ParseLsof(out)

	want := []scan.Process{{PID: 101, Command: "zsh", Cwd: "/Users/me/wt"}, {PID: 202, Command: "claude code", Cwd: "/tmp/a b"}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %+v\nwant %+v", got, want)
	}
}
