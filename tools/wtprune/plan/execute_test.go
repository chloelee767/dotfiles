package plan_test

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"path/filepath"
	"strings"
	"testing"

	"wtprune/plan"
	"wtprune/scan"
)

func TestExecute_RemovesCleanWorktree(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	want := "removing 1 worktree...\nremoved   " + w.Path + "\n---\n1 removed, 0 trashed, 0 branches deleted, 0 skipped, 0 failed   log: " + opts.LogPath + "\n"
	if out.String() != want {
		t.Errorf("output:\n%s\nwant:\n%s", out.String(), want)
	}
	if exists(w.Path) || strings.Contains(run(t, repo, "git", "worktree", "list"), w.Path) {
		t.Error("worktree dir or registration still exists")
	}
	if sum.ExitCode() != 0 {
		t.Errorf("exit code = %d, want 0", sum.ExitCode())
	}
	if log := read(t, opts.LogPath); !strings.Contains(log, `"action":"removed"`) || !strings.Contains(log, `"head":"`+w.Head+`"`) {
		t.Errorf("log = %s, want a removed entry with the HEAD sha", log)
	}
}

func TestExecute_TrashesAtRiskWorktree(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", "notes.md"), "precious\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	trashed := filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912")
	if want := "removing 1 worktree...\ntrashed   " + w.Path + " -> " + trashed + "\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if got := read(t, filepath.Join(trashed, "notes.md")); got != "precious\n" {
		t.Errorf("trashed notes.md = %q, want the untracked file preserved", got)
	}
	if exists(w.Path) || strings.Contains(run(t, repo, "git", "worktree", "list"), w.Path) {
		t.Error("worktree dir or registration still exists")
	}
	if sum.Trashed != 1 || sum.Failed != 0 {
		t.Errorf("summary = %+v, want 1 trashed", sum)
	}
	if log := read(t, opts.LogPath); !strings.Contains(log, `"changes":["?? notes.md"]`) || !strings.Contains(log, `"trash":"`+trashed+`"`) {
		t.Errorf("log = %s, want the changed paths and trash location", log)
	}
}

func TestExecute_TrashesIgnoredFilesOfSafeWorktree(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", ".env"), "SECRET=1\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)

	plan.Execute(context.Background(), p, io.Discard, opts)

	if got := read(t, filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912", ".env")); got != "SECRET=1\n" {
		t.Errorf("trashed .env = %q, want it preserved", got)
	}
}

func TestExecute_TrashNameCollision(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a", "wt"), "-b", "a")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "b", "wt"), "-b", "b")
	write(t, filepath.Join(root, "a", "wt", "x"), "a\n")
	write(t, filepath.Join(root, "b", "wt", "x"), "b\n")
	res := scanRepo(t, repo)
	var ids []string
	for _, w := range res.Rows {
		ids = append(ids, w.ID)
	}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids}, false)
	opts := testOptions(t)

	sum := plan.Execute(context.Background(), p, io.Discard, opts)

	if sum.Trashed != 2 || !exists(filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912")) || !exists(filepath.Join(opts.TrashDir, "wtprune-wt-20261017-0912-2")) {
		t.Errorf("summary = %+v, want two trashed dirs, the second suffixed -2", sum)
	}
}

func TestExecute_NoTrashForcesRemoval(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", "notes.md"), "n\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, true)
	opts := testOptions(t)
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, opts)

	if want := "removing 1 worktree...\nremoved   " + w.Path + " (forced)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if exists(w.Path) || exists(opts.TrashDir) {
		t.Error("worktree still exists, or something went to the Trash")
	}
}

func TestExecute_SkipsWorktreeChangedSinceScan(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	write(t, filepath.Join(root, "wt", "late.txt"), "written after the scan\n")
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if want := "removing 1 worktree and 1 branch...\nskipped   " + w.Path + ": changed since scan (now DIRTY 1?)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if !exists(filepath.Join(root, "wt", "late.txt")) {
		t.Error("late.txt was removed")
	}
	if run(t, repo, "git", "branch", "--list", "feat") == "" {
		t.Error("branch feat was deleted although its worktree was skipped")
	}
	if sum.Skipped != 1 || sum.ExitCode() != 0 {
		t.Errorf("summary = %+v, want 1 skipped and exit 0", sum)
	}
}

func TestExecute_SkipsWorktreeLockedSinceScan(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	write(t, filepath.Join(root, "wt", "notes.md"), "n\n")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	run(t, repo, "git", "worktree", "lock", w.Path)
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, testOptions(t))

	if want := "removing 1 worktree...\nskipped   " + w.Path + ": changed since scan (now locked)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if !exists(w.Path) {
		t.Error("locked worktree was moved or removed")
	}
}

func TestExecute_SkipsWorktreeNewlyInUse(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)
	opts.Processes = func(context.Context) ([]scan.Process, error) {
		return []scan.Process{{PID: 4242, Command: "zsh", Cwd: w.Path}}, nil
	}
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, opts)

	if want := "removing 1 worktree...\nskipped   " + w.Path + ": changed since scan (now IN USE zsh)\n"; !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
}

// Guards the -c status.showUntrackedFiles=normal override: with the repo
// config set to "no", a plain `git worktree remove` deletes untracked files.
func TestRemoveSafely_RefusesUntrackedDespiteRepoConfig(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, repo, "git", "config", "status.showUntrackedFiles", "no")
	write(t, filepath.Join(wt, "notes.md"), "precious\n")

	err := plan.RemoveSafely(context.Background(), filepath.Join(repo, ".git"), wt)

	if err == nil || !strings.Contains(err.Error(), "untracked") {
		t.Errorf("err = %v, want git to refuse because of untracked files", err)
	}
	if read(t, filepath.Join(wt, "notes.md")) != "precious\n" {
		t.Error("notes.md changed")
	}
}

func TestExecute_DeletesPointerBranchAfterRemoval(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "switch", "-q", "-c", "feat-x")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "someone's PR")
	run(t, repo, "git", "push", "-q", "origin", "feat-x")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "feat-x")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "review"), "-b", "review-818", "origin/feat-x")
	res := scanRepo(t, repo)
	w := row(t, res, "review")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	want := "removing 1 worktree and 1 branch...\nremoved   " + w.Path + "\ndeleted   branch review-818 in repo (was " + w.Head[:7] + ")\n"
	if !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if run(t, repo, "git", "branch", "--list", "review-818") != "" || sum.BranchesDeleted != 1 {
		t.Errorf("branch still exists or summary %+v", sum)
	}
}

func TestExecute_SkipsBranchWhoseTipMoved(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	run(t, filepath.Join(root, "wt"), "git", "commit", "-q", "--allow-empty", "-m", "feat")
	run(t, repo, "git", "merge", "-q", "--ff-only", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)
	// As if the scan had seen an older tip than the branch has now.
	p.Branches[0].Tip = run(t, repo, "git", "rev-parse", "feat~1")
	var out bytes.Buffer

	plan.Execute(context.Background(), p, &out, testOptions(t))

	want := "removing 1 worktree and 1 branch...\nremoved   " + w.Path + "\nskipped   branch feat in repo: tip moved since scan\n"
	if !strings.HasPrefix(out.String(), want) {
		t.Errorf("output:\n%s\nwant prefix:\n%s", out.String(), want)
	}
	if run(t, repo, "git", "branch", "--list", "feat") == "" {
		t.Error("branch feat was deleted")
	}
}

// Build already drops one of a mutually-containing pair; Execute must still
// catch it if handed both (eg. the refs changed between plan and execute).
func TestExecute_MutualContainmentKeepsOneBranch(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "switch", "-q", "-c", "tmp")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "only on a and b")
	run(t, repo, "git", "branch", "a")
	run(t, repo, "git", "branch", "b")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "tmp")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a"), "a")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "b"), "b")
	res := scanRepo(t, repo)
	a, b := row(t, res, "a"), row(t, res, "b")
	if !a.Branch.Candidate() || !b.Branch.Candidate() {
		t.Fatalf("setup: a %+v b %+v, want both candidates (each contains the other)", a.Branch, b.Branch)
	}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{a.ID, b.ID}}, false)
	for _, w := range []*scan.Worktree{a, b} {
		p.Branches = append(p.Branches, plan.BranchAction{Repo: w.Repo, Name: w.Branch.Name, Tip: w.Branch.Tip, Tier: w.Branch.Tier, WorktreeID: w.ID})
	}
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, testOptions(t))

	if sum.BranchesDeleted != 1 || sum.Skipped != 1 || !strings.Contains(out.String(), "1 commits would be lost") {
		t.Errorf("summary %+v, output:\n%s\nwant one branch deleted and one kept", sum, out.String())
	}
	if run(t, repo, "git", "branch", "--list", "a", "b") == "" {
		t.Error("both branches were deleted: the commit is lost")
	}
}

func TestExecute_FailureDoesNotStopTheRest(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "dirty"), "-b", "dirty")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "clean"), "-b", "clean")
	write(t, filepath.Join(root, "dirty", "notes.md"), "n\n")
	res := scanRepo(t, repo)
	dirty, clean := row(t, res, "dirty"), row(t, res, "clean")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{dirty.ID, clean.ID}}, false)
	opts := testOptions(t)
	opts.TrashDir = filepath.Join(root, "repo", "README", "not-a-dir")
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if !strings.Contains(out.String(), "\nFAILED    "+dirty.Path+": move to Trash (nothing removed): ") {
		t.Errorf("output:\n%s\nwant the at-risk worktree to fail", out.String())
	}
	if !exists(filepath.Join(root, "dirty", "notes.md")) || exists(clean.Path) {
		t.Error("want dirty untouched and clean removed")
	}
	if sum.Failed != 1 || sum.Removed != 1 || sum.ExitCode() != 1 {
		t.Errorf("summary = %+v, want 1 failed, 1 removed, exit 1", sum)
	}
}

// cancelOnRemoved cancels once a "removed" line is written.
type cancelOnRemoved struct {
	w      io.Writer
	cancel context.CancelFunc
}

func (c cancelOnRemoved) Write(p []byte) (int, error) {
	if strings.HasPrefix(string(p), "removed") {
		c.cancel()
	}
	return c.w.Write(p)
}

func TestExecute_CancelFinishesCurrentActionThenStops(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a"), "-b", "a")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "b"), "-b", "b")
	res := scanRepo(t, repo)
	ids := []string{row(t, res, "a").ID, row(t, res, "b").ID}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids}, false)
	opts := testOptions(t)
	opts.Workers = 1
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var out bytes.Buffer

	sum := plan.Execute(ctx, p, cancelOnRemoved{w: &out, cancel: cancel}, opts)

	if sum.Removed != 1 || sum.NotAttempted != 1 || !strings.Contains(out.String(), "1 not attempted (interrupted)") {
		t.Errorf("summary %+v, output:\n%s\nwant 1 removed then stop", sum, out.String())
	}
	if notice := strings.Index(out.String(), "\ninterrupted: "); notice < 0 || notice > strings.Index(out.String(), "\n---\n") {
		t.Errorf("output:\n%s\nwant an interrupted notice before the summary", out.String())
	}
	if sum.ExitCode() != 1 {
		t.Errorf("exit code = %d, want 1 when items were not attempted", sum.ExitCode())
	}
}

func TestExecute_CancelFinishesInFlightActionsInParallel(t *testing.T) {
	root, repo := newRepo(t)
	var ids, paths []string
	for _, name := range []string{"a", "b", "c", "d", "e", "f"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), "-b", name)
		paths = append(paths, filepath.Join(root, name))
	}
	res := scanRepo(t, repo)
	for _, w := range res.Rows {
		ids = append(ids, w.ID)
	}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids}, false)
	opts := testOptions(t)
	opts.Workers = 2
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var out bytes.Buffer

	sum := plan.Execute(ctx, p, cancelOnRemoved{w: &out, cancel: cancel}, opts)

	left := 0
	for _, path := range paths {
		if exists(path) {
			left++
		}
	}
	// The cancel lands while both workers are busy: each finishes its
	// worktree, then nothing new starts.
	if sum.Removed < 1 || sum.Removed > 2 || sum.Removed+sum.NotAttempted != 6 || left != sum.NotAttempted || sum.Failed != 0 {
		t.Errorf("summary %+v with %d worktrees left, output:\n%s\nwant the in-flight removals done and the rest not attempted", sum, left, out.String())
	}
}

func TestExecute_LogUnwritableDoesNothing(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "feat")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")
	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}}, false)
	opts := testOptions(t)
	opts.LogPath = filepath.Join(root, "repo", "README", "log")
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if !exists(w.Path) || sum.ExitCode() != 1 || sum.Removed != 0 {
		t.Errorf("summary = %+v, want nothing removed without an action log", sum)
	}
	if want := "could not open log " + opts.LogPath + ": "; !strings.HasPrefix(out.String(), want) || !strings.HasSuffix(out.String(), "; nothing done\n") || strings.Contains(out.String(), "interrupted") {
		t.Errorf("output = %q, want %q...; nothing done", out.String(), want)
	}
}

func TestExecute_ParallelAcrossReposAndWorktrees(t *testing.T) {
	root, repoA := newRepo(t)
	repoB := filepath.Join(root, "repo-b")
	run(t, root, "git", "clone", "-q", repoA, repoB)
	var rows []*scan.Worktree
	var ids []string
	for _, repo := range []string{repoA, repoB} {
		for _, name := range []string{"one", "two", "three"} {
			wt := filepath.Join(root, filepath.Base(repo)+"-"+name)
			run(t, repo, "git", "worktree", "add", "-q", wt, "-b", name)
			run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", name)
			run(t, repo, "git", "merge", "-q", "--ff-only", name)
		}
		write(t, filepath.Join(root, filepath.Base(repo)+"-two", "notes.md"), "precious\n")
		for _, w := range scanRepo(t, repo).Rows {
			rows = append(rows, w)
			ids = append(ids, w.ID)
		}
	}
	p := plan.Build(rows, plan.Selection{Worktrees: ids, Branches: ids}, false)
	opts := testOptions(t)
	opts.Workers = 4
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if sum.Removed != 4 || sum.Trashed != 2 || sum.BranchesDeleted != 6 || sum.Failed+sum.Skipped+sum.NotAttempted != 0 {
		t.Errorf("summary %+v, output:\n%s\nwant 4 removed, 2 trashed, 6 branches deleted", sum, out.String())
	}
	lines := strings.Split(strings.TrimSpace(out.String()), "\n")
	if len(lines) != 1+6+6+2 || lines[0] != "removing 6 worktrees and 6 branches, 4 at a time..." {
		t.Errorf("output:\n%s\nwant a header, one line per action, and the summary", out.String())
	}
	for _, repo := range []string{repoA, repoB} {
		if got := run(t, repo, "git", "branch", "--list", "one", "two", "three"); got != "" {
			t.Errorf("%s still has branches %q", repo, got)
		}
		if got := run(t, repo, "git", "worktree", "list", "--porcelain"); strings.Count(got, "worktree ") != 1 {
			t.Errorf("%s still has linked worktrees:\n%s", repo, got)
		}
	}
	if log := read(t, opts.LogPath); strings.Count(log, "\n") != 6+6+2 {
		t.Errorf("log has %d lines, want 14 (6 removals, 6 branches, 2 trashing intents):\n%s", strings.Count(log, "\n"), log)
	}
}

// The batch check under concurrency: three branches share one unique
// commit, so at least one must survive, whichever order the workers run in.
func TestExecute_ParallelBatchCheckKeepsSharedCommit(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "switch", "-q", "-c", "tmp")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "only on a, b and c")
	shared := run(t, repo, "git", "rev-parse", "HEAD")
	for _, name := range []string{"a", "b", "c"} {
		run(t, repo, "git", "branch", name)
	}
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "tmp")
	for _, name := range []string{"a", "b", "c"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), name)
	}
	res := scanRepo(t, repo)
	var ids []string
	for _, w := range res.Rows {
		ids = append(ids, w.ID)
	}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids}, false)
	// Bypass Build's own batch check, as if refs changed after planning.
	for _, w := range res.Rows {
		p.Branches = append(p.Branches, plan.BranchAction{Repo: w.Repo, Name: w.Branch.Name, Tip: w.Branch.Tip, Tier: w.Branch.Tier, WorktreeID: w.ID})
	}
	opts := testOptions(t)
	opts.Workers = 3
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if run(t, repo, "git", "branch", "--contains", shared) == "" {
		t.Fatalf("every branch holding %s was deleted; output:\n%s", shared, out.String())
	}
	if sum.Removed != 3 || sum.BranchesDeleted+sum.Skipped != 3 || sum.Skipped < 1 {
		t.Errorf("summary %+v, output:\n%s\nwant 3 removed and at least one branch kept", sum, out.String())
	}
}

// Branches of several repos are abandoned at once (every worktree is
// skipped): run with -race.
func TestExecute_AbandonsBranchesAcrossReposConcurrently(t *testing.T) {
	root, repoA := newRepo(t)
	var rows []*scan.Worktree
	var ids []string
	for i := range 4 {
		repo := repoA
		if i > 0 {
			repo = filepath.Join(root, fmt.Sprintf("repo%d", i))
			run(t, root, "git", "clone", "-q", repoA, repo)
		}
		for _, name := range []string{"one", "two", "three"} {
			run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, filepath.Base(repo)+"-"+name), "-b", name)
		}
		for _, w := range scanRepo(t, repo).Rows {
			rows = append(rows, w)
			ids = append(ids, w.ID)
		}
		for _, name := range []string{"one", "two", "three"} {
			run(t, repo, "git", "worktree", "lock", filepath.Join(root, filepath.Base(repo)+"-"+name))
		}
	}
	p := plan.Build(rows, plan.Selection{Worktrees: ids, Branches: ids}, false)
	opts := testOptions(t)
	opts.Workers = 4

	sum := plan.Execute(context.Background(), p, io.Discard, opts)

	if len(p.Branches) != 12 || sum.Skipped != 12 || sum.BranchesDeleted != 0 {
		t.Errorf("summary %+v with %d planned branches, want all 12 worktrees skipped and no branch deleted", sum, len(p.Branches))
	}
}

// Many worktrees of one repo removed 4 at a time, each with its branch,
// some via the Trash: git commands racing in one repo must not turn into
// FAILED or skipped lines.
func TestExecute_ParallelWithinOneRepo(t *testing.T) {
	root, repo := newRepo(t)
	for i := range 16 {
		wt := filepath.Join(root, fmt.Sprintf("b%02d", i))
		run(t, repo, "git", "worktree", "add", "-q", wt, "-b", fmt.Sprintf("b%02d", i))
		if i%3 == 0 {
			write(t, filepath.Join(wt, ".env"), "x\n")
		}
	}
	res := scanRepo(t, repo)
	var ids []string
	for _, w := range res.Rows {
		ids = append(ids, w.ID)
	}
	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids, Branches: ids}, false)
	opts := testOptions(t)
	opts.Workers = 4
	var out bytes.Buffer

	sum := plan.Execute(context.Background(), p, &out, opts)

	if sum.Failed+sum.Skipped > 0 || sum.Removed != 10 || sum.Trashed != 6 || sum.BranchesDeleted != 16 {
		t.Errorf("summary %+v, output:\n%s\nwant 10 removed, 6 trashed, 16 branches deleted", sum, out.String())
	}
}
