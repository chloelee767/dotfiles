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

	if want := "removing 1 worktree...\nskipped   " + w.Path + ": changed since scan (HEAD moved to " + head + "; now clean)\n"; !strings.HasPrefix(out.String(), want) {
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

	if want := "removing 1 worktree...\nskipped   " + w.Path + ": changed since scan (now BISECTING)\n"; !strings.HasPrefix(out.String(), want) {
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

	if want := "removing 1 worktree...\nskipped   " + w.Path + ": changed since scan (now clean +1 ignored)\n"; !strings.HasPrefix(out.String(), want) {
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
	opts.Workers = 1
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
	if want := "removing 1 worktree and 1 branch...\nFAILED    " + w.Path + ": moved to " + dest + " but deregistering failed: "; !strings.HasPrefix(out.String(), want) {
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

	if !strings.HasPrefix(out.String(), "removing 1 worktree and 1 branch...\nFAILED    "+w.Path+": ") || sum.BranchesDeleted != 0 {
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

	if want := "removing 1 worktree and 1 branch...\nremoved   " + w.Path + "\nskipped   branch gone in repo: no longer exists\n"; !strings.HasPrefix(out.String(), want) {
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

	if want := "removing 1 worktree...\nremoved   " + w.Path + "\n"; !strings.HasPrefix(out.String(), want) || sum.Removed != 1 {
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

// A detached worktree at an intermediate commit of a squash-merged branch:
// the scan counts the commit as safe in the branch, but this run deletes
// the branch. Build drops such a worktree; Execute must still catch it if
// handed both (eg. the refs changed between plan and execute), whichever
// runs first.
func TestExecute_DetachedHeadOnlyInBranchBeingDeletedIsKept(t *testing.T) {
	for _, tc := range []struct {
		name    string
		workers int
		dirty   string // the dirty worktree goes first: at-risk items run before safe ones
	}{
		{"branch first, sequential", 1, "x"},
		{"detached first, sequential", 1, "w"},
		{"branch first, parallel", 4, "x"},
		{"detached first, parallel", 4, "w"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			root, repo := newRepo(t)
			x, w := filepath.Join(root, "x"), filepath.Join(root, "w")
			run(t, repo, "git", "worktree", "add", "-q", x, "-b", "x")
			write(t, filepath.Join(x, "f"), "one\n")
			run(t, x, "git", "add", "f")
			run(t, x, "git", "commit", "-qm", "c1")
			c1 := run(t, x, "git", "rev-parse", "HEAD")
			write(t, filepath.Join(x, "f"), "two\n")
			run(t, x, "git", "commit", "-qam", "c2")
			run(t, repo, "git", "merge", "-q", "--squash", "x")
			run(t, repo, "git", "commit", "-qm", "squash x")
			run(t, repo, "git", "worktree", "add", "-q", "--detach", w, c1)
			write(t, filepath.Join(root, tc.dirty, "scratch.txt"), "at risk, so first\n")
			res := scanRepo(t, repo)
			if row(t, res, "x").Branch.Tier != scan.TierSquashed || row(t, res, "w").DetachedLost != 0 {
				t.Fatalf("setup: x %+v, w lost %d", row(t, res, "x").Branch, row(t, res, "w").DetachedLost)
			}
			xRow := row(t, res, "x")
			p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{xRow.ID, row(t, res, "w").ID}}, false)
			p.Branches = append(p.Branches, plan.BranchAction{
				Repo: xRow.Repo, Name: "x", Tip: xRow.Branch.Tip, Tier: xRow.Branch.Tier, WorktreeID: xRow.ID,
				SquashCommit: xRow.Branch.SquashCommit, SquashRef: xRow.Branch.SquashRef,
			})
			opts := testOptions(t)
			opts.Workers = tc.workers
			var out bytes.Buffer

			plan.Execute(context.Background(), p, &out, opts)

			if !exists(w) || run(t, w, "git", "rev-parse", "HEAD") != c1 {
				t.Errorf("output:\n%s\nwant the detached worktree kept at %s", out.String(), c1)
			}
			if !strings.Contains(out.String(), "skipped   "+row(t, res, "w").Path+": ") {
				t.Errorf("output:\n%s\nwant the detached worktree skipped", out.String())
			}
		})
	}
}

func TestExecute_SkipsDetachedHeadOnlyInBranchBeingDeleted(t *testing.T) {
	root, repo := newRepo(t)
	x, w := filepath.Join(root, "x"), filepath.Join(root, "w")
	run(t, repo, "git", "worktree", "add", "-q", x, "-b", "x")
	run(t, x, "git", "commit", "-q", "--allow-empty", "-m", "c1")
	c1 := run(t, x, "git", "rev-parse", "HEAD")
	run(t, x, "git", "commit", "-q", "--allow-empty", "-m", "c2")
	run(t, repo, "git", "branch", "y", "x")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", w, c1)
	res := scanRepo(t, repo)
	wRow := row(t, res, "w")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{wRow.ID}}, false)
	// As if x and y were planned too: only they hold c1.
	for _, name := range []string{"x", "y"} {
		p.Branches = append(p.Branches, plan.BranchAction{Repo: wRow.Repo, Name: name, Tip: c1, WorktreeID: "elsewhere-" + name})
	}
	opts := testOptions(t)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	want := "removing 1 worktree and 2 branches...\nskipped   " + wRow.Path + ": holds 1 commit only x, y have (being deleted); remove it next run\n"
	if !strings.HasPrefix(out.String(), want) || sum.Skipped != 1 || !exists(w) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
}

func TestExecute_StaleOrigHeadDoesNotSkipDetachedWorktree(t *testing.T) {
	root, repo := newRepo(t)
	w := filepath.Join(root, "w")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", w)
	run(t, w, "git", "commit", "-q", "--allow-empty", "-m", "dropped by reset")
	run(t, w, "git", "reset", "-q", "--hard", "HEAD~1")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "x"), "-b", "x")
	res := scanRepo(t, repo)
	ids := []string{row(t, res, "w").ID, row(t, res, "x").ID}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids, Branches: ids}, false)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if sum.Removed != 2 || sum.BranchesDeleted != 1 || sum.Skipped != 0 || exists(w) {
		t.Errorf("summary %+v, output:\n%s\nwant both worktrees removed and x deleted", sum, out.String())
	}
}
