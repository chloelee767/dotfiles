package plan_test

import (
	"path/filepath"
	"reflect"
	"slices"
	"testing"

	"wtprune/plan"
)

func TestBuild_AtRiskFirstAndBlockedDropped(t *testing.T) {
	root, repo := newRepo(t)
	for _, name := range []string{"clean", "dirty", "env", "locked"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), "-b", name)
	}
	write(t, filepath.Join(root, "dirty", "notes.md"), "n\n")
	write(t, filepath.Join(root, "env", ".env"), "SECRET=1\n")
	run(t, repo, "git", "worktree", "lock", filepath.Join(root, "locked"))
	res := scanRepo(t, repo)
	clean, dirty, env, locked := row(t, res, "clean"), row(t, res, "dirty"), row(t, res, "env"), row(t, res, "locked")

	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{clean.ID, dirty.ID, env.ID, locked.ID}}, false)

	if want := []plan.Item{{W: dirty, Trash: true}}; !reflect.DeepEqual(p.AtRisk, want) {
		t.Errorf("at risk = %+v, want dirty via Trash", p.AtRisk)
	}
	if len(p.Safe) != 2 || !slices.Contains(p.Safe, plan.Item{W: clean}) || !slices.Contains(p.Safe, plan.Item{W: env, Trash: true}) {
		t.Errorf("safe = %+v, want clean (plain) and env (via Trash)", p.Safe)
	}
	if want := []plan.Dropped{{Kind: "worktree", Name: locked.Path, Reason: "blocked: locked"}}; !reflect.DeepEqual(p.Dropped, want) {
		t.Errorf("dropped = %+v, want %+v", p.Dropped, want)
	}
	if !p.NeedsTypedYes() {
		t.Error("NeedsTypedYes = false, want true with an at-risk item")
	}
	if want := []string{"DIRTY: ?? notes.md"}; !reflect.DeepEqual(p.AtRisk[0].Reasons(), want) {
		t.Errorf("reasons = %q, want %q", p.AtRisk[0].Reasons(), want)
	}
}

func TestBuild_BranchesNeedTheirWorktreeAndCandidacy(t *testing.T) {
	root, repo := newRepo(t)
	for _, name := range []string{"merged", "unique", "unselected"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), "-b", name)
	}
	run(t, filepath.Join(root, "unique"), "git", "commit", "-q", "--allow-empty", "-m", "only here")
	res := scanRepo(t, repo)
	merged, unique, unselected := row(t, res, "merged"), row(t, res, "unique"), row(t, res, "unselected")

	p := plan.Build(res.Rows, plan.Selection{
		Worktrees: []string{merged.ID, unique.ID},
		Branches:  []string{merged.ID, unique.ID, unselected.ID},
	}, false)

	want := []plan.BranchAction{{
		Repo: merged.Repo, Name: "merged", Tip: merged.Head, Tier: "merged", Label: "merged (in main)", WorktreeID: merged.ID,
	}}
	if !reflect.DeepEqual(p.Branches, want) {
		t.Errorf("branches = %+v\nwant %+v", p.Branches, want)
	}
	wantDropped := []plan.Dropped{
		{Kind: "branch", Name: "unique", Reason: "keep: 1 unique"},
		{Kind: "branch", Name: "unselected", Reason: "its worktree is not being removed"},
	}
	if !reflect.DeepEqual(p.Dropped, wantDropped) {
		t.Errorf("dropped = %+v\nwant %+v", p.Dropped, wantDropped)
	}
	if p.NeedsTypedYes() {
		t.Error("NeedsTypedYes = true, want y for safe rows and a merged branch")
	}
}

func TestBuild_PushedBranchNeedsTypedYes(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "mine")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "my work")
	run(t, wt, "git", "push", "-q", "-u", "origin", "mine")
	res := scanRepo(t, repo)
	w := row(t, res, "wt")

	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{w.ID}, Branches: []string{w.ID}}, false)

	if len(p.Branches) != 1 || !p.Branches[0].NeedsTypedYes() || !p.NeedsTypedYes() {
		t.Errorf("branches = %+v, want one pushed branch requiring typed yes", p.Branches)
	}
}

func TestBuild_NoTrash(t *testing.T) {
	root, repo := newRepo(t)
	for _, name := range []string{"dirty", "env"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), "-b", name)
	}
	write(t, filepath.Join(root, "dirty", "notes.md"), "n\n")
	write(t, filepath.Join(root, "env", ".env"), "SECRET=1\n")
	res := scanRepo(t, repo)
	dirty, env := row(t, res, "dirty"), row(t, res, "env")

	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{dirty.ID, env.ID}}, true)

	if want := []plan.Item{{W: dirty, Force: true}}; !reflect.DeepEqual(p.AtRisk, want) {
		t.Errorf("at risk = %+v, want dirty forced", p.AtRisk)
	}
	// Plain removal deletes ignored files anyway; no need for --force.
	if want := []plan.Item{{W: env}}; !reflect.DeepEqual(p.Safe, want) {
		t.Errorf("safe = %+v, want env removed plainly", p.Safe)
	}
}

func TestBuild_Empty(t *testing.T) {
	_, repo := newRepo(t)

	p := plan.Build(scanRepo(t, repo).Rows, plan.Selection{}, false)

	if !p.Empty() {
		t.Errorf("plan = %+v, want empty", p)
	}
}

func TestBuild_MutualContainmentDropsOneBranchAtPlanTime(t *testing.T) {
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
	ids := []string{a.ID, b.ID}

	p := plan.Build(res.Rows, plan.Selection{Worktrees: ids, Branches: ids}, false)

	if len(p.Branches) != 1 {
		t.Fatalf("branches = %+v, want exactly one kept for deletion", p.Branches)
	}
	dropped := p.Branches[0].Name
	if dropped == "a" {
		dropped = "b"
	} else {
		dropped = "a"
	}
	want := []plan.Dropped{{Kind: "branch", Name: dropped, Reason: "only other ref is also being deleted (would lose 1)"}}
	if !reflect.DeepEqual(p.Dropped, want) {
		t.Errorf("dropped = %+v, want %+v", p.Dropped, want)
	}
	for _, it := range p.Items() {
		if it.DeleteBranch != (it.W.Branch.Name == p.Branches[0].Name) {
			t.Errorf("item %s DeleteBranch = %v, want only the kept branch's worktree", it.W.Branch.Name, it.DeleteBranch)
		}
	}
}

func TestBuild_DropsDetachedWorktreeWhoseCommitOnlyAPlannedBranchHolds(t *testing.T) {
	root, repo := newRepo(t)
	x := filepath.Join(root, "x")
	run(t, repo, "git", "worktree", "add", "-q", x, "-b", "x")
	write(t, filepath.Join(x, "f"), "one\n")
	run(t, x, "git", "add", "f")
	run(t, x, "git", "commit", "-qm", "c1")
	c1 := run(t, x, "git", "rev-parse", "HEAD")
	write(t, filepath.Join(x, "f"), "two\n")
	run(t, x, "git", "commit", "-qam", "c2")
	run(t, repo, "git", "merge", "-q", "--squash", "x")
	run(t, repo, "git", "commit", "-qm", "squash x")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", filepath.Join(root, "w"), c1)
	res := scanRepo(t, repo)
	xRow, wRow := row(t, res, "x"), row(t, res, "w")

	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{xRow.ID, wRow.ID}, Branches: []string{xRow.ID}}, false)

	want := []plan.Dropped{{Kind: "worktree", Name: wRow.Path, Reason: "holds 1 commit only x has (being deleted); unflag it (b), or remove this worktree next run"}}
	if !reflect.DeepEqual(p.Dropped, want) || len(p.Items()) != 1 || len(p.Branches) != 1 {
		t.Errorf("dropped = %+v, items %d, branches %+v; want w dropped, x and its branch kept", p.Dropped, len(p.Items()), p.Branches)
	}
}

func TestBuild_IgnoredDirsDecideTrash(t *testing.T) {
	root, repo := newRepo(t)
	write(t, filepath.Join(repo, ".git", "info", "exclude"), "data/\nnode_modules/\n")
	for _, name := range []string{"deps", "data"} {
		run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, name), "-b", name)
	}
	write(t, filepath.Join(root, "deps", "node_modules", "x", "index.js"), "x")
	write(t, filepath.Join(root, "data", "data", "fixtures.db"), "precious")
	res := scanRepo(t, repo)
	deps, data := row(t, res, "deps"), row(t, res, "data")

	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{deps.ID, data.ID}}, false)

	if len(p.Safe) != 2 || !slices.Contains(p.Safe, plan.Item{W: deps}) || !slices.Contains(p.Safe, plan.Item{W: data, Trash: true}) {
		t.Errorf("safe = %+v, want deps removed plainly and data via Trash", p.Safe)
	}
}

// A leftover ORIG_HEAD (here from a reset) is history, not an in-progress
// operation's head: it must not hold the worktree back.
func TestBuild_StaleOrigHeadDoesNotDropDetachedWorktree(t *testing.T) {
	root, repo := newRepo(t)
	w := filepath.Join(root, "w")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", w)
	run(t, w, "git", "commit", "-q", "--allow-empty", "-m", "dropped by reset")
	run(t, w, "git", "reset", "-q", "--hard", "HEAD~1")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "x"), "-b", "x")
	res := scanRepo(t, repo)
	wRow, xRow := row(t, res, "w"), row(t, res, "x")

	p := plan.Build(res.Rows, plan.Selection{Worktrees: []string{wRow.ID, xRow.ID}, Branches: []string{xRow.ID}}, false)

	if len(p.Dropped) != 0 || len(p.Items()) != 2 || len(p.Branches) != 1 {
		t.Errorf("dropped = %+v, items %d, branches %d; want both worktrees and x's branch", p.Dropped, len(p.Items()), len(p.Branches))
	}
}
