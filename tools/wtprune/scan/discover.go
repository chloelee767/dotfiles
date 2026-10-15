package scan

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"wtprune/git"
)

// Skipped counts what --dir discovery saw but does not list.
type Skipped struct {
	Clones     int      // .git dirs: main worktrees / full clones
	ClonePaths []string // their paths
	OtherGit   int      // .git files that aren't linked worktrees, eg. submodule checkouts
	NonGit     int      // top-level dirs with no git checkout within --depth
}

// discoverRepo returns the linked worktrees of the repo containing path.
func discoverRepo(ctx context.Context, path string) ([]*Worktree, []*Repo, error) {
	common, err := git.Run(ctx, path, "rev-parse", "--path-format=absolute", "--git-common-dir")
	if err != nil {
		return nil, nil, fmt.Errorf("%s is not inside a git repository (use --dir to scan a folder of worktrees): %w", path, err)
	}
	repo, err := loadRepo(ctx, strings.TrimSpace(common))
	if err != nil {
		return nil, nil, err
	}
	var rows []*Worktree
	for i, reg := range repo.registered {
		if i == 0 || reg.entry.Bare {
			continue
		}
		rows = append(rows, registeredRow(repo, reg))
	}
	return rows, []*Repo{repo}, nil
}

// registeredRow builds the row for a registered linked worktree, marking it
// missing when its dir is gone or no longer points back at its admin dir.
func registeredRow(repo *Repo, reg registration) *Worktree {
	e := reg.entry
	w := &Worktree{
		ID:         reg.canonical,
		Path:       reg.path,
		Repo:       repo,
		AdminDir:   reg.adminDir,
		Head:       e.Head,
		Detached:   e.Detached,
		Locked:     e.Locked,
		LockReason: e.LockReason,
	}
	w.Branch.Name = strings.TrimPrefix(e.Branch, "refs/heads/")
	switch {
	case e.Prunable || !exists(reg.path):
		w.State = StateMissing
		w.Hint = "dir is gone: git worktree prune (repo-wide) drops it; if it was moved, git worktree repair <new-path>"
	case reg.adminDir == "":
		w.State = StateBroken
		w.Hint = "no admin dir under " + filepath.Join(repo.CommonDir, "worktrees")
	default:
		gitdir, err := readGitFile(reg.path)
		if err != nil || pathKey(Canonical(gitdir)) != pathKey(Canonical(reg.adminDir)) {
			w.State = StateBroken
			w.Hint = fmt.Sprintf("%s/.git does not point at %s", reg.path, reg.adminDir)
		}
	}
	if e.Locked && (w.State == StateOK || w.State == StateMissing) {
		w.State = StateLocked
		w.Hint = "git worktree unlock " + reg.path
	}
	return w
}

type candidate struct {
	dir   string // canonical
	admin string // <common>/worktrees/<id>, as written in the .git file
}

// discoverDir walks root up to depth looking for linked worktrees, across
// repos. It stops descending at any dir containing .git.
func discoverDir(ctx context.Context, root string, depth int) ([]*Worktree, []*Repo, Skipped, error) {
	root = Canonical(root)
	if info, err := os.Stat(root); err != nil || !info.IsDir() {
		return nil, nil, Skipped{}, fmt.Errorf("--dir %s: not a directory", root)
	}
	var skipped Skipped
	var candidates []candidate

	// found reports whether any .git was seen at or under dir.
	var visit func(dir string, level int) bool
	visit = func(dir string, level int) bool {
		dotGit := filepath.Join(dir, ".git")
		if info, err := os.Lstat(dotGit); err == nil {
			switch {
			case info.IsDir():
				skipped.Clones++
				skipped.ClonePaths = append(skipped.ClonePaths, dir)
			case isLinkedWorktreeGitFile(dir):
				gitdir, _ := readGitFile(dir)
				candidates = append(candidates, candidate{dir: dir, admin: gitdir})
			default:
				skipped.OtherGit++
			}
			return true
		}
		if level >= depth {
			return false
		}
		entries, err := os.ReadDir(dir)
		if err != nil {
			return false
		}
		found := false
		for _, e := range entries {
			if !e.IsDir() || e.Name() == "node_modules" {
				continue
			}
			sub := filepath.Join(dir, e.Name())
			if visit(sub, level+1) {
				found = true
			} else if level == 0 {
				skipped.NonGit++
			}
		}
		return found
	}
	visit(root, 0)

	repos := map[string]*Repo{}
	var order []*Repo
	var rows []*Worktree
	for _, c := range candidates {
		if !exists(c.admin) {
			common := filepath.Dir(filepath.Dir(c.admin))
			rows = append(rows, &Worktree{
				ID:    c.dir,
				Path:  c.dir,
				Repo:  &Repo{Name: repoName("", common), CommonDir: common},
				State: StateBroken,
				Hint:  ".git points at " + c.admin + ", which is gone",
			})
			continue
		}
		common, err := readRelFile(c.admin, "commondir")
		if err != nil {
			common = filepath.Dir(filepath.Dir(c.admin))
		}
		key := pathKey(Canonical(common))
		repo, ok := repos[key]
		if !ok {
			repo, err = loadRepo(ctx, common)
			if err != nil {
				rows = append(rows, &Worktree{
					ID: c.dir, Path: c.dir, Repo: &Repo{Name: repoName("", Canonical(common)), CommonDir: Canonical(common)},
					State: StateError, Hint: err.Error(),
				})
				continue
			}
			repos[key] = repo
			order = append(order, repo)
		}
		reg, ok := repo.registrationFor(c.admin)
		if !ok {
			rows = append(rows, &Worktree{
				ID: c.dir, Path: c.dir, Repo: repo, AdminDir: c.admin,
				State: StateBroken, Hint: c.admin + " is not a registered worktree of " + repo.CommonDir,
			})
			continue
		}
		if pathKey(reg.canonical) != pathKey(c.dir) {
			rows = append(rows, &Worktree{
				ID: c.dir, Path: c.dir, Repo: repo, AdminDir: reg.adminDir,
				Head: reg.entry.Head, Detached: reg.entry.Detached,
				Branch: Branch{Name: strings.TrimPrefix(reg.entry.Branch, "refs/heads/")},
				State:  StateMoved,
				Hint:   fmt.Sprintf("registered at %s: run `git worktree repair` in %s", reg.path, c.dir),
			})
			continue
		}
		rows = append(rows, registeredRow(repo, reg))
	}
	return rows, order, skipped, nil
}

// isLinkedWorktreeGitFile reports whether dir/.git is a gitdir file pointing
// at <common>/worktrees/<id> (as opposed to eg. a submodule's modules dir).
func isLinkedWorktreeGitFile(dir string) bool {
	gitdir, err := readGitFile(dir)
	if err != nil {
		return false
	}
	return filepath.Base(filepath.Dir(gitdir)) == "worktrees"
}

// registeredPaths returns the canonical path of every worktree (main and
// linked) of the given repos, for nested-worktree detection.
func registeredPaths(repos []*Repo) []string {
	var paths []string
	for _, r := range repos {
		for _, reg := range r.registered {
			if !reg.entry.Bare {
				paths = append(paths, reg.canonical)
			}
		}
	}
	slices.Sort(paths)
	return slices.Compact(paths)
}
