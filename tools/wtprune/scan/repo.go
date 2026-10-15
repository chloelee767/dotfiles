package scan

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"wtprune/git"
)

var alwaysProtected = []string{"main", "master", "develop", "trunk"}

type registration struct {
	path      string // as git reports it
	canonical string
	adminDir  string // <common>/worktrees/<id>; empty for the main worktree
	entry     git.WorktreeEntry
}

// loadRepo lists a repo's worktrees and the refs that make a branch
// protected. commonDir need not be canonical.
func loadRepo(ctx context.Context, commonDir string) (*Repo, error) {
	common := Canonical(commonDir)
	out, err := git.Run(ctx, common, "worktree", "list", "--porcelain", "-z")
	if err != nil {
		return nil, err
	}
	entries := git.ParseWorktreeList(out)

	repo := &Repo{CommonDir: common, protected: map[string]bool{}}
	admins := adminDirsByPath(common)
	for i, e := range entries {
		reg := registration{path: e.Path, canonical: Canonical(e.Path), entry: e}
		if i > 0 {
			reg.adminDir, _ = admins.get(reg.canonical)
		} else if !e.Bare {
			repo.MainPath = e.Path
		}
		repo.registered = append(repo.registered, reg)
	}
	repo.Name = repoName(repo.MainPath, common)

	for _, b := range alwaysProtected {
		repo.protected[b] = true
	}
	refs, err := git.Run(ctx, common, "for-each-ref", "--format=%(refname)%00%(symref)", "refs/remotes", "refs/heads")
	if err != nil {
		return nil, err
	}
	lines := strings.Split(strings.TrimSpace(refs), "\n")
	exists := map[string]bool{}
	for _, line := range lines {
		ref, _, _ := strings.Cut(line, "\x00")
		exists[ref] = true
	}
	var heads []string
	for _, line := range lines {
		ref, target, _ := strings.Cut(line, "\x00")
		if strings.HasPrefix(ref, "refs/heads/") {
			heads = append(heads, ref)
			continue
		}
		remotePrefix, ok := strings.CutSuffix(ref, "HEAD")
		if !ok || target == "" || !strings.HasPrefix(target, remotePrefix) {
			continue
		}
		repo.protected[strings.TrimPrefix(target, remotePrefix)] = true
		if exists[target] {
			repo.defaultBranches = append(repo.defaultBranches, target)
		}
	}
	for _, ref := range heads {
		if repo.protected[strings.TrimPrefix(ref, "refs/heads/")] {
			repo.defaultBranches = append(repo.defaultBranches, ref)
		}
	}
	return repo, nil
}

// adminDirsByPath maps each linked worktree's registered path to its admin
// dir, by reading <common>/worktrees/<id>/gitdir.
func adminDirsByPath(common string) *pathIndex[string] {
	admins := newPathIndex[string]()
	dirs, _ := os.ReadDir(filepath.Join(common, "worktrees"))
	for _, d := range dirs {
		admin := filepath.Join(common, "worktrees", d.Name())
		dotGit, err := readRelFile(admin, "gitdir")
		if err != nil {
			continue
		}
		admins.add(Canonical(filepath.Dir(dotGit)), admin)
	}
	return admins
}

// repoName is the main worktree's dir name, or for a bare repo the common
// dir's name without ".git".
func repoName(mainPath, commonDir string) string {
	if mainPath != "" {
		return filepath.Base(mainPath)
	}
	name := filepath.Base(commonDir)
	if name == ".git" {
		return filepath.Base(filepath.Dir(commonDir))
	}
	return strings.TrimSuffix(name, ".git")
}

// registrationFor finds the linked worktree registration whose admin dir is
// admin.
func (r *Repo) registrationFor(admin string) (registration, bool) {
	ix := newPathIndex[registration]()
	for _, reg := range r.registered {
		if reg.adminDir != "" {
			ix.add(Canonical(reg.adminDir), reg)
		}
	}
	return ix.get(Canonical(admin))
}

// registrationAt finds the linked worktree registered at canonical path p.
func (r *Repo) registrationAt(p string) (registration, bool) {
	ix := newPathIndex[registration]()
	for i, reg := range r.registered {
		if i > 0 {
			ix.add(reg.canonical, reg)
		}
	}
	return ix.get(p)
}

// pathIndex looks up canonical paths exactly, falling back to a unique
// case-folded match only when nothing matches exactly: macOS volumes are
// usually case-insensitive, but two dirs that differ only in case on a
// case-sensitive volume must never be conflated.
type pathIndex[V any] struct {
	exact  map[string]V
	folded map[string][]V
}

func newPathIndex[V any]() *pathIndex[V] {
	return &pathIndex[V]{exact: map[string]V{}, folded: map[string][]V{}}
}

func (ix *pathIndex[V]) add(p string, v V) {
	ix.exact[p] = v
	ix.folded[pathKey(p)] = append(ix.folded[pathKey(p)], v)
}

func (ix *pathIndex[V]) get(p string) (V, bool) {
	if v, ok := ix.exact[p]; ok {
		return v, true
	}
	if vs := ix.folded[pathKey(p)]; len(vs) == 1 {
		return vs[0], true
	}
	var zero V
	return zero, false
}

// pathKey case-folds a canonical path on macOS.
func pathKey(p string) string {
	if runtime.GOOS == "darwin" {
		return strings.ToLower(p)
	}
	return p
}

func shortRef(ref string) string {
	for _, prefix := range []string{"refs/heads/", "refs/remotes/", "refs/tags/"} {
		if s, ok := strings.CutPrefix(ref, prefix); ok {
			return s
		}
	}
	return ref
}
