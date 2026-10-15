package scan_test

import (
	"os"
	"path/filepath"
	"testing"

	"wtprune/scan"
)

func TestBranch_MergedIntoMain(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "feat work")
	run(t, repo, "git", "merge", "-q", "--ff-only", "feat")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierMerged || b.Label() != "merged (in main)" || !b.Bulk() {
		t.Errorf("branch = %+v label %q, want bulk merged (in main)", b, b.Label())
	}
}

func TestBranch_MergedIntoRemoteDefault(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "feat work")
	run(t, wt, "git", "push", "-q", "origin", "feat:main")
	run(t, repo, "git", "fetch", "-q", "origin")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierMerged || b.Label() != "merged (in origin/main)" {
		t.Errorf("label = %q, want merged (in origin/main)", b.Label())
	}
}

func TestBranch_PointerToRemoteBranch(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "switch", "-q", "-c", "feat-x")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "someone's PR")
	run(t, repo, "git", "push", "-q", "origin", "feat-x")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "feat-x")
	wt := filepath.Join(root, "review")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "review-818", "origin/feat-x")

	b := row(t, scanRepo(t, repo), "review").Branch

	if b.Tier != scan.TierPointer || b.Label() != "pointer (= origin/feat-x)" || !b.Bulk() {
		t.Errorf("branch = %+v label %q, want bulk pointer (= origin/feat-x)", b, b.Label())
	}
}

func TestBranch_PushedButNotMerged(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "chloe/old")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "my work")
	run(t, wt, "git", "push", "-q", "-u", "origin", "chloe/old")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierPushed || b.Label() != "pushed, not merged" || b.Bulk() {
		t.Errorf("branch = %+v label %q, want non-bulk pushed", b, b.Label())
	}
	if b.ContainedIn != "origin/chloe/old" || b.Upstream != "origin/chloe/old" {
		t.Errorf("contained in %q upstream %q, want origin/chloe/old", b.ContainedIn, b.Upstream)
	}
}

func TestBranch_MissingReflogCountsAsPushed(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "switch", "-q", "-c", "feat-x")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "remote only")
	run(t, repo, "git", "push", "-q", "origin", "feat-x")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "feat-x")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "wt"), "-b", "review", "origin/feat-x")
	if err := os.Remove(filepath.Join(repo, ".git", "logs", "refs", "heads", "review")); err != nil {
		t.Fatal(err)
	}

	if b := row(t, scanRepo(t, repo), "wt").Branch; b.Tier != scan.TierPushed {
		t.Errorf("tier = %q, want pushed when the reflog is missing", b.Tier)
	}
}

// Guards the --exclude bug: --exclude=refs/heads/<b> excludes nothing, so
// the branch would "contain itself" and look deletable.
func TestBranch_UniqueCommitIsKept(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "only here")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Candidate() || b.Label() != "1 unique" {
		t.Errorf("branch = %+v label %q, want keep: 1 unique", b, b.Label())
	}
}

func TestBranch_ProtectedNames(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "branch", "develop")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "dev"), "develop")
	run(t, repo, "git", "push", "-q", "origin", "main:release")
	run(t, repo, "git", "remote", "set-head", "origin", "release")
	run(t, repo, "git", "branch", "release")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "rel"), "release")

	res := scanRepo(t, repo)

	for _, name := range []string{"dev", "rel"} {
		if b := row(t, res, name).Branch; b.Candidate() || b.KeepReason != "protected" {
			t.Errorf("%s: branch = %+v, want keep: protected", name, b)
		}
	}
}

func TestBranch_CheckedOutElsewhere(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "branch", "feat")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(root, "a"), "feat")
	run(t, repo, "git", "worktree", "add", "-q", "--force", filepath.Join(root, "b"), "feat")

	res := scanRepo(t, repo)

	for _, name := range []string{"a", "b"} {
		if b := row(t, res, name).Branch; b.Candidate() || b.KeepReason != "in use elsewhere" {
			t.Errorf("%s: branch = %+v, want keep: in use elsewhere", name, b)
		}
	}
}

func TestBranch_ContainedInNonDefaultBranch(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "commit", "-q", "--allow-empty", "-m", "feat work")
	run(t, repo, "git", "branch", "bigger", "feat")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierContained || b.Label() != "contained (in bigger)" || !b.Bulk() {
		t.Errorf("branch = %+v label %q, want bulk contained (in bigger)", b, b.Label())
	}
}

func TestBranch_DetachedHasNoLabel(t *testing.T) {
	root, repo := newRepo(t)
	run(t, repo, "git", "worktree", "add", "-q", "--detach", filepath.Join(root, "wt"))

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Label() != "" || b.KeepReason != "detached" {
		t.Errorf("label %q keep reason %q, want no label and keep reason detached", b.Label(), b.KeepReason)
	}
}

// pointerBranch makes root/review on branch review, a fresh branch at
// origin/feat-x whose only commit was authored elsewhere.
func pointerBranch(t *testing.T) (root, repo, wt string) {
	t.Helper()
	root, repo = newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "switch", "-q", "-c", "feat-x")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "someone's PR")
	run(t, repo, "git", "push", "-q", "origin", "feat-x")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "feat-x")
	wt = filepath.Join(root, "review")
	run(t, repo, "git", "worktree", "add", "-q", "--track", wt, "-b", "review", "origin/feat-x")
	return root, repo, wt
}

// pushMoreToFeatX adds a commit to origin/feat-x from outside the worktree.
func pushMoreToFeatX(t *testing.T, repo string) {
	t.Helper()
	run(t, repo, "git", "switch", "-q", "-c", "tmp", "origin/feat-x")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "more PR work")
	run(t, repo, "git", "push", "-q", "origin", "tmp:feat-x")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "tmp")
}

func TestBranch_PullFastForwardIsStillPointer(t *testing.T) {
	_, repo, wt := pointerBranch(t)
	pushMoreToFeatX(t, repo)
	run(t, wt, "git", "pull", "-q", "--ff-only")

	b := row(t, scanRepo(t, repo), "review").Branch

	if b.Tier != scan.TierPointer {
		t.Errorf("tier = %q (reflog: %s), want pointer", b.Tier, run(t, repo, "git", "reflog", "show", "--format=%gs", "refs/heads/review"))
	}
}

func TestBranch_ResetToRemoteIsStillPointer(t *testing.T) {
	_, repo, wt := pointerBranch(t)
	pushMoreToFeatX(t, repo)
	run(t, repo, "git", "fetch", "-q", "origin")
	run(t, wt, "git", "checkout", "-q", "-B", "review", "origin/feat-x")

	b := row(t, scanRepo(t, repo), "review").Branch

	if b.Tier != scan.TierPointer {
		t.Errorf("tier = %q (reflog: %s), want pointer", b.Tier, run(t, repo, "git", "reflog", "show", "--format=%gs", "refs/heads/review"))
	}
}

func TestBranch_OwnCommitsByEmailArePushedNotPointer(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	run(t, repo, "git", "config", "user.email", "TEST@example.com")
	run(t, repo, "git", "switch", "-q", "-c", "mine")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "my work")
	run(t, repo, "git", "push", "-q", "origin", "mine")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "branch", "-q", "-D", "mine")
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "again")
	run(t, wt, "git", "reset", "-q", "--hard", "origin/mine")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierPushed {
		t.Errorf("tier = %q, want pushed: the commit's author email is user.email", b.Tier)
	}
}

func TestBranch_SquashMergedIntoMain(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "README"), "hello\nfeat 1\n")
	run(t, wt, "git", "commit", "-qam", "feat 1")
	write(t, filepath.Join(wt, "new.txt"), "feat 2\n")
	run(t, wt, "git", "add", "new.txt")
	run(t, wt, "git", "commit", "-qm", "feat 2")
	write(t, filepath.Join(repo, "other.txt"), "main moved on\n")
	run(t, repo, "git", "add", "other.txt")
	run(t, repo, "git", "commit", "-qm", "main work")
	run(t, repo, "git", "merge", "-q", "--squash", "feat")
	run(t, repo, "git", "commit", "-qm", "feat (#12)")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierSquashed || b.Label() != "squash-merged (into main)" || !b.Bulk() {
		t.Errorf("branch = %+v label %q, want bulk squash-merged (into main)", b, b.Label())
	}
	if b.SquashCommit != run(t, repo, "git", "rev-parse", "main") || b.SquashRef != "refs/heads/main" {
		t.Errorf("squash commit %q ref %q, want main's tip", b.SquashCommit, b.SquashRef)
	}
}

func TestBranch_PushedThenSquashMergedIntoRemoteDefault(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "new.txt"), "feat\n")
	run(t, wt, "git", "add", "new.txt")
	run(t, wt, "git", "commit", "-qm", "feat")
	run(t, wt, "git", "push", "-q", "-u", "origin", "feat")
	run(t, repo, "git", "switch", "-q", "--detach")
	run(t, repo, "git", "merge", "-q", "--squash", "feat")
	run(t, repo, "git", "commit", "-qm", "feat (#12)")
	run(t, repo, "git", "push", "-q", "origin", "HEAD:main")
	run(t, repo, "git", "switch", "-q", "main")
	run(t, repo, "git", "fetch", "-q", "origin")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier != scan.TierSquashed || b.Label() != "squash-merged (into origin/main)" {
		t.Errorf("branch = %+v label %q, want squash-merged (into origin/main)", b, b.Label())
	}
}

func TestBranch_UnmergedUniqueCommitIsNotSquashed(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "new.txt"), "feat\n")
	run(t, wt, "git", "add", "new.txt")
	run(t, wt, "git", "commit", "-qm", "feat")
	write(t, filepath.Join(repo, "new.txt"), "different\n")
	run(t, repo, "git", "add", "new.txt")
	run(t, repo, "git", "commit", "-qm", "same file, other content")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Candidate() || b.Label() != "1 unique" {
		t.Errorf("branch = %+v label %q, want keep: 1 unique", b, b.Label())
	}
}

func TestBranch_EmptyNetDiffIsNotSquashed(t *testing.T) {
	root, repo := newRepo(t)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "README"), "changed\n")
	run(t, wt, "git", "commit", "-qam", "change")
	run(t, wt, "git", "revert", "--no-edit", "HEAD")
	run(t, repo, "git", "commit", "-q", "--allow-empty", "-m", "main moved on")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Candidate() || b.Label() != "2 unique" {
		t.Errorf("branch = %+v label %q, want keep: 2 unique", b, b.Label())
	}
}

func TestBranch_UpstreamGone(t *testing.T) {
	root, repo := newRepo(t)
	addOrigin(t, root, repo)
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	run(t, wt, "git", "push", "-q", "-u", "origin", "feat")
	run(t, repo, "git", "push", "-q", "origin", "--delete", "feat")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if !b.UpstreamGone || b.Upstream != "origin/feat" {
		t.Errorf("branch = %+v, want upstream origin/feat marked gone", b)
	}
}

// squashedYAMLBranch squash-merges feat (adding "d: 3" to y.yaml) into main
// and returns the feat worktree.
func squashedYAMLBranch(t *testing.T) (repo, wt string) {
	t.Helper()
	root, repo := newRepo(t)
	write(t, filepath.Join(repo, "y.yaml"), "a: 1\nb: 2\nc: 3\n")
	run(t, repo, "git", "add", "y.yaml")
	run(t, repo, "git", "commit", "-qm", "yaml")
	wt = filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "y.yaml"), "a: 1\nb: 2\nc: 3\nd: 3\n")
	run(t, wt, "git", "commit", "-qam", "add d")
	run(t, repo, "git", "merge", "-q", "--squash", "feat")
	run(t, repo, "git", "commit", "-qm", "add d (#1)")
	return repo, wt
}

func TestBranch_WhitespaceFollowUpIsNotSquashed(t *testing.T) {
	repo, wt := squashedYAMLBranch(t)
	write(t, filepath.Join(wt, "y.yaml"), "a: 1\nb: 2\nc: 3\n  d: 3\n")
	run(t, wt, "git", "commit", "-qam", "indent d (unpushed)")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier == scan.TierSquashed {
		t.Errorf("branch = %+v, want kept: the indentation change is not in main", b)
	}
}

func TestBranch_LineMoveFollowUpIsNotSquashed(t *testing.T) {
	root, repo := newRepo(t)
	block := "p\nq\nr\ns\nt\nu\n"
	write(t, filepath.Join(repo, "f"), block+block)
	run(t, repo, "git", "add", "f")
	run(t, repo, "git", "commit", "-qm", "f")
	wt := filepath.Join(root, "wt")
	run(t, repo, "git", "worktree", "add", "-q", wt, "-b", "feat")
	write(t, filepath.Join(wt, "f"), "p\nq\nr\nX\ns\nt\nu\n"+block)
	run(t, wt, "git", "commit", "-qam", "insert X")
	run(t, repo, "git", "merge", "-q", "--squash", "feat")
	run(t, repo, "git", "commit", "-qm", "insert X (#1)")
	write(t, filepath.Join(wt, "f"), block+"p\nq\nr\nX\ns\nt\nu\n")
	run(t, wt, "git", "commit", "-qam", "move X down (unpushed)")

	b := row(t, scanRepo(t, repo), "wt").Branch

	if b.Tier == scan.TierSquashed {
		t.Errorf("branch = %+v, want kept: X's new position is not in main", b)
	}
}

func TestBranch_SquashMergedYAMLStillDetected(t *testing.T) {
	repo, _ := squashedYAMLBranch(t)

	if b := row(t, scanRepo(t, repo), "wt").Branch; b.Tier != scan.TierSquashed {
		t.Errorf("branch = %+v, want squash-merged", b)
	}
}
