package scan_test

import (
	"path/filepath"
	"reflect"
	"testing"

	"wtprune/scan"
)

func TestDiscoverDir_AcrossReposSkippingClonesAndOtherGitFiles(t *testing.T) {
	root, repo := newRepo(t)
	_, other := newRepo(t)
	wts := filepath.Join(root, "wts")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "repo-a"), "-b", "a")
	run(t, other, "git", "worktree", "add", "-q", filepath.Join(wts, "group", "other-b"), "-b", "b")
	run(t, root, "git", "clone", "-q", repo, filepath.Join(wts, "a-clone"))
	run(t, root, "git", "init", "-q", "--separate-git-dir", filepath.Join(root, "sep.git"), filepath.Join(wts, "separate"))
	write(t, filepath.Join(wts, "notes", "todo.txt"), "x\n")

	res := scanDir(t, wts, 3)

	if got := names(res); len(got) != 2 {
		t.Fatalf("rows = %v, want repo-a and other-b", got)
	}
	if len(res.Repos) != 2 || row(t, res, "repo-a").Repo == row(t, res, "other-b").Repo {
		t.Errorf("repos = %d, want the two worktrees grouped into two repos", len(res.Repos))
	}
	if want := (scan.Skipped{Clones: 1, ClonePaths: []string{canonical(t, filepath.Join(wts, "a-clone"))}, OtherGit: 1, NonGit: 1}); !reflect.DeepEqual(res.Skipped, want) {
		t.Errorf("skipped = %+v, want %+v", res.Skipped, want)
	}
	if !res.DirMode || res.Target != canonical(t, wts) {
		t.Errorf("dir mode %v target %q", res.DirMode, res.Target)
	}
}

func TestDiscoverDir_GroupsWorktreesOfOneRepo(t *testing.T) {
	root, repo := newRepo(t)
	wts := filepath.Join(root, "wts")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "a"), "-b", "a")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "b"), "-b", "b")

	res := scanDir(t, wts, 3)

	if len(res.Repos) != 1 || row(t, res, "a").Repo != row(t, res, "b").Repo {
		t.Errorf("repos = %d, want one shared repo", len(res.Repos))
	}
}

func TestDiscoverDir_RespectsDepth(t *testing.T) {
	root, repo := newRepo(t)
	wts := filepath.Join(root, "wts")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "shallow"), "-b", "shallow")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "x", "y", "deep"), "-b", "deep")

	if got := names(scanDir(t, wts, 2)); !reflect.DeepEqual(got, []string{"shallow"}) {
		t.Errorf("depth 2: rows = %v, want only shallow", got)
	}
	if got := names(scanDir(t, wts, 3)); len(got) != 2 {
		t.Errorf("depth 3: rows = %v, want both", got)
	}
}

func TestDiscoverDir_SkipsNodeModules(t *testing.T) {
	root, repo := newRepo(t)
	wts := filepath.Join(root, "wts")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "node_modules", "pkg"), "-b", "pkg")

	if got := names(scanDir(t, wts, 3)); len(got) != 0 {
		t.Errorf("rows = %v, want none under node_modules", got)
	}
}

func TestDiscoverDir_DoesNotDescendIntoWorktrees(t *testing.T) {
	root, repo := newRepo(t)
	wts := filepath.Join(root, "wts")
	outer := filepath.Join(wts, "outer")
	run(t, repo, "git", "worktree", "add", "-q", outer, "-b", "outer")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(outer, "build", "inner"), "-b", "inner")

	res := scanDir(t, wts, 3)

	if got := names(res); !reflect.DeepEqual(got, []string{"outer"}) {
		t.Errorf("rows = %v, want only outer", got)
	}
	// The inner worktree is still known from the repo's worktree list.
	if w := row(t, res, "outer"); w.State != scan.StateNested {
		t.Errorf("outer state = %q, want nested", w.State)
	}
}

func TestDiscoverDir_ThroughSymlinkedRoot(t *testing.T) {
	root, repo := newRepo(t)
	wts := filepath.Join(root, "wts")
	run(t, repo, "git", "worktree", "add", "-q", filepath.Join(wts, "a"), "-b", "a")
	link := filepath.Join(root, "link")
	run(t, root, "ln", "-s", wts, link)

	res := scanDir(t, link, 3)

	if w := row(t, res, "a"); !w.Selectable() || w.ID != canonical(t, filepath.Join(wts, "a")) {
		t.Errorf("state %q id %q, want a selectable row keyed by canonical path", w.State, w.ID)
	}
}
