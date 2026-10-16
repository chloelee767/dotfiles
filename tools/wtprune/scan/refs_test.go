package scan_test

import (
	"context"
	"path/filepath"
	"testing"

	"wtprune/git"
	"wtprune/scan"
)

// The scan and CountUnreachableNow skip the full-ref walk when a ref points
// at the tip or a default branch contains it. These cases must still agree
// with git.CountUnreachable (the walk).

func TestCountUnreachableNow_OnlyExcludedBranchAtTipStillCounts(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "unique")
	tip := run(t, wt, "git", "rev-parse", "HEAD")
	res := scanRepo(t, repo)
	r := row(t, res, "wt").Repo

	n, err := scan.CountUnreachableNow(context.Background(), r, tip, []string{"feat"})
	walked, walkErr := git.CountUnreachable(context.Background(), repo, tip, []string{"feat"})

	if err != nil || walkErr != nil || n != 1 || walked != 1 {
		t.Errorf("CountUnreachableNow = %d, %v; walk = %d, %v; want both 1", n, err, walked, walkErr)
	}
	if b := row(t, res, "wt").Branch; b.KeepReason != "1 unique" {
		t.Errorf("scan branch = %+v, want 1 unique", b)
	}
}

func TestCountUnreachableNow_SymrefAtTipIsNotAFastProof(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "only feat")
	tip := run(t, wt, "git", "rev-parse", "HEAD")
	// rev-list --remotes resolves the symref to feat's tip, so the walk
	// counts 0 even with feat excluded; only the walk may decide that.
	run(t, repo, "git", "symbolic-ref", "refs/remotes/origin/HEAD", "refs/heads/feat")
	r := row(t, scanRepo(t, repo), "wt").Repo

	n, err := scan.CountUnreachableNow(context.Background(), r, tip, []string{"feat"})
	walked, walkErr := git.CountUnreachable(context.Background(), repo, tip, []string{"feat"})

	if err != nil || walkErr != nil || n != walked {
		t.Errorf("CountUnreachableNow = %d, %v; walk = %d, %v; want them equal", n, err, walked, walkErr)
	}
}

func TestCountUnreachableNow_AnnotatedTagAtTip(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", "--detach", wt)
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "released")
	tip := run(t, wt, "git", "rev-parse", "HEAD")
	run(t, repo, "git", "tag", "-a", "-m", "v1", "v1", tip)
	w := row(t, scanRepo(t, repo), "wt")

	n, err := scan.CountUnreachableNow(context.Background(), w.Repo, tip, nil)

	if err != nil || n != 0 || w.DetachedLost != 0 {
		t.Errorf("CountUnreachableNow = %d, %v; scan DetachedLost = %d; want 0 (the tag holds it)", n, err, w.DetachedLost)
	}
}

func TestCountUnreachableNow_DefaultBranchContainsTip(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "merged")
	tip := run(t, wt, "git", "rev-parse", "HEAD")
	run(t, repo, "git", "merge", "-q", "--ff-only", "feat")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "after")
	r := row(t, scanRepo(t, repo), "wt").Repo

	n, err := scan.CountUnreachableNow(context.Background(), r, tip, []string{"feat"})

	if err != nil || n != 0 {
		t.Errorf("CountUnreachableNow = %d, %v; want 0 (main contains it)", n, err)
	}
}

func TestBranch_ContainedInAnnotatedTagAtTip(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "released")
	run(t, repo, "git", "tag", "-a", "-m", "v1", "v1", "feat")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierContained || b.ContainedIn != "v1" || !b.SameTip {
		t.Errorf("branch = %+v, want contained (in v1) at the same tip", b)
	}
}

func TestBranch_OtherBranchAtTipIsContained(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "shared")
	run(t, repo, "git", "branch", "copy", "feat")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierContained || b.ContainedIn != "copy" || !b.SameTip {
		t.Errorf("branch = %+v, want contained (in copy) at the same tip", b)
	}
}

// Containment counts only branches and tags: a remote-tracking ref at the
// tip makes it a pointer, not contained.
func TestBranch_RemoteAtTipIsPointerNotContained(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "pushed")
	run(t, wt, "git", "push", "-q", "origin", "feat:review")
	run(t, repo, "git", "fetch", "-q", "origin")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierPushed || b.ContainedIn != "origin/review" || !b.SameTip {
		t.Errorf("branch = %+v, want pushed with origin/review at the same tip", b)
	}
}
