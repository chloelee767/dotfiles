package plan_test

import (
	"bytes"
	"context"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"wtprune/plan"
	"wtprune/scan"
)

func TestExecute_TrashKeepsCopyOfAdminDir(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", "notes.md"), "precious\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	head := read(t, filepath.Join(w.AdminDir, "HEAD"))
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)

	sum := plan.Execute(context.Background(), p, io.Discard, opts)

	gitdir := filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912", ".wtprune-gitdir")
	if sum.Trashed != 1 || read(t, filepath.Join(gitdir, "HEAD")) != head || !exists(filepath.Join(gitdir, "logs", "HEAD")) {
		t.Errorf("summary %+v, want the admin dir (HEAD, logs) copied to %s", sum, gitdir)
	}
	if exists(w.AdminDir) {
		t.Error("admin dir still exists: worktree not deregistered")
	}
	log := read(t, opts.LogPath)
	trashing := strings.Index(log, `"action":"trashing"`)
	trashed := strings.Index(log, `"action":"trashed"`)
	if trashing < 0 || trashed < trashing || !strings.Contains(log, `"gitdir":"`+gitdir+`"`) {
		t.Errorf("log = %s, want a trashing intent line, then trashed, with the gitdir copy", log)
	}
}

func TestExecute_LogsLostCommitsOfDetachedHead(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", wt)
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "lost 1")
	lost1 := run(t, wt, "git", "rev-parse", "HEAD")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "lost 2")
	lost2 := run(t, wt, "git", "rev-parse", "HEAD")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)

	plan.Execute(context.Background(), p, io.Discard, opts)

	if log := read(t, opts.LogPath); !strings.Contains(log, `"lost":["`+lost2+`","`+lost1+`"]`) {
		t.Errorf("log = %s, want both lost shas", log)
	}
}

func TestExecute_LogsReflogOnlyCommits(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "dropped by reset")
	dropped := run(t, wt, "git", "rev-parse", "HEAD")
	run(t, wt, "git", "reset", "-q", "--hard", "HEAD~1")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	opts := testOptions(t)

	sum := plan.Execute(context.Background(), p, io.Discard, opts)

	log := read(t, opts.LogPath)
	if sum.Removed != 1 || sum.BranchesDeleted != 1 || strings.Count(log, `"reflog_only":["`+dropped+`"]`) != 2 {
		t.Errorf("summary %+v log = %s, want the reflog-only sha on both the removal and the branch deletion", sum, log)
	}
}

func TestExecute_SkipsWhenHeadMoved(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "after the scan")
	head := run(t, wt, "git", "rev-parse", "--short=7", "HEAD")
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, testOptions(t))

	if want := "skipped   " + w.Path + ": changed since scan (HEAD moved to " + head + "; now clean)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if !exists(wt) {
		t.Error("worktree was removed")
	}
}

func TestExecute_SkipsWhenOperationStarted(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	run(t, wt, "git", "bisect", "start")
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, testOptions(t))

	if want := "skipped   " + w.Path + ": changed since scan (now BISECTING)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
}

func TestExecute_SkipsWhenIgnoredFileAdded(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	write(t, filepath.Join(wt, ".env"), "SECRET=1\n")
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, testOptions(t))

	if want := "skipped   " + w.Path + ": changed since scan (now clean +1 ignored)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if read(t, filepath.Join(wt, ".env")) != "SECRET=1\n" {
		t.Error(".env changed")
	}
}

func TestExecute_ProcessesRelistedPerItem(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a"), "-b", "a")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "b"), "-b", "b")
	res := scanRepo(t, repo)
	a, b := row(t, res, "a"), row(t, res, "b")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{a.ID, b.ID}}, false)
	opts := testOptions(t)
	calls := 0
	opts.Processes = func(context.Context) ([]scan.Process, error) {
		calls++
		if calls == 2 {
			// A shell opened in the second worktree while the first was removed.
			return []scan.Process{{PID: 4242, Command: "zsh", Cwd: p.Items()[1].W.Path}}, nil
		}
		return nil, nil
	}
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if sum.Removed != 1 || sum.Skipped != 1 || !strings.Contains(out.String(), "(now IN USE zsh)") {
		t.Errorf("summary %+v, output:\n%s\nwant the second worktree skipped as newly in use", sum, out.String())
	}
}

func TestExecute_RenamedButDeregisterFailed(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", "notes.md"), "precious\n")
	run(t, filepath.Join(root, "wt"), "git", "commit", "-q", "--allow-empty", "-m", "x")
	run(t, repo, "git", "merge", "-q", "--ff-only", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	if err := os.Chmod(w.AdminDir, 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.Chmod(w.AdminDir, 0o755) })
	opts := testOptions(t)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	dest := filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912")
	if want := "FAILED    " + w.Path + ": moved to " + dest + " but deregistering failed: "; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if read(t, filepath.Join(dest, "notes.md")) != "precious\n" || run(t, repo, "git", "branch", "--list", "feat") == "" {
		t.Error("want notes.md in the Trash and branch feat kept")
	}
	if sum.Failed != 1 || sum.BranchesDeleted != 0 {
		t.Errorf("summary = %+v, want 1 failed and no branch deleted", sum)
	}
}

func TestExecute_RemovalFailedKeepsBranch(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "sub", "f"), "x\n")
	run(t, wt, "git", "add", ".")
	run(t, wt, "git", "commit", "-qm", "sub")
	run(t, repo, "git", "merge", "-q", "--ff-only", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	if err := os.Chmod(filepath.Join(wt, "sub"), 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.Chmod(filepath.Join(wt, "sub"), 0o755) })
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if !strings.HasPrefix(out.String(), "FAILED    "+w.Path+": ") || sum.BranchesDeleted != 0 {
		t.Errorf("summary %+v output:\n%s\nwant the removal to fail and no branch deleted", sum, out.String())
	}
	if run(t, repo, "git", "branch", "--list", "feat") == "" {
		t.Error("branch feat was deleted although its worktree removal failed")
	}
}

func TestExecute_SkipsBranchThatNoLongerExists(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	p.Branches[0].Name = "gone"
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if want := "removed   " + w.Path + "\nskipped   branch gone in repo: no longer exists\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if sum.Skipped != 1 || sum.ExitCode() != 0 {
		t.Errorf("summary = %+v, want 1 skipped, exit 0", sum)
	}
}

func TestExecute_NoTrashRemovesIgnoredOnlyRowPlainly(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", ".env"), "SECRET=1\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, true)
	opts := testOptions(t)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if want := "removed   " + w.Path + "\n"; !strings.HasPrefix(out.String(), want) || sum.Removed != 1 {
		t.Errorf("summary %+v output:\n%s\nwant a plain removal", sum, out.String())
	}
	if exists(w.Path) || exists(opts.TrashDir) {
		t.Error("worktree still exists, or something went to the Trash")
	}
}

func TestExecute_UnsearchableTrashDirFailsInsteadOfHanging(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", "notes.md"), "precious\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)
	if err := os.MkdirAll(opts.TrashDir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(opts.TrashDir, 0o000); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.Chmod(opts.TrashDir, 0o700) })
	done := make(chan plan.Summary)

	go func() { done <- plan.Execute(context.Background(), p, io.Discard, opts) }()

	select {
	case sum := <-done:
		if sum.Failed != 1 || !exists(filepath.Join(root, "wt", "notes.md")) {
			t.Errorf("summary = %+v, want 1 failed and the worktree untouched", sum)
		}
	case <-time.After(20 * time.Second):
		t.Fatal("Execute hung picking a Trash path")
	}
}

func TestExecute_DeletesSquashMergedBranch(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "new.txt"), "feat\n")
	run(t, wt, "git", "add", "new.txt")
	run(t, wt, "git", "commit", "-qm", "feat")
	run(t, repo, "git", "merge", "-q", "--squash", "feat")
	run(t, repo, "git", "commit", "-qm", "feat (#12)")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	if w.Branch.Tier != scan.TierSquashed {
		t.Fatalf("setup: branch %+v, want squashed", w.Branch)
	}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if sum.BranchesDeleted != 1 || run(t, repo, "git", "branch", "--list", "feat") != "" {
		t.Errorf("summary %+v output:\n%s\nwant the squash-merged branch deleted", sum, out.String())
	}
}

func TestExecute_SkipsSquashedBranchWhoseSquashCommitIsGone(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "new.txt"), "feat\n")
	run(t, wt, "git", "add", "new.txt")
	run(t, wt, "git", "commit", "-qm", "feat")
	run(t, repo, "git", "merge", "-q", "--squash", "feat")
	run(t, repo, "git", "commit", "-qm", "feat (#12)")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	run(t, repo, "git", "reset", "-q", "--hard", "HEAD~1")
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if !strings.Contains(out.String(), "skipped   branch feat in repo: squash commit no longer in main") || run(t, repo, "git", "branch", "--list", "feat") == "" {
		t.Errorf("summary %+v output:\n%s\nwant the branch kept", sum, out.String())
	}
}

func TestExecute_TrashedGitFileIsDisarmed(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a", "wt"), "-b", "feat")
	write(t, filepath.Join(root, "a", "wt", "notes.md"), "precious\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)

	sum := plan.Execute(context.Background(), p, io.Discard, opts)

	dest := filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912")
	if sum.Trashed != 1 || exists(filepath.Join(dest, ".git")) || !exists(filepath.Join(dest, ".git.wtprune-orig")) {
		t.Fatalf("summary %+v, want .git renamed to .git.wtprune-orig in %s", sum, dest)
	}
	// Re-adding under the same basename reuses the freed admin dir id.
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "b", "wt"), "-b", "again")
	cmd := exec.Command("git", "status")
	cmd.Dir = dest
	if out, err := cmd.CombinedOutput(); err == nil {
		t.Errorf("git status in the trash dir succeeded, want not-a-repo:\n%s", out)
	}
}
