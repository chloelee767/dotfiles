package scan

import (
	"cmp"
	"context"
	"slices"
	"strings"
	"sync"

	"wtprune/git"
)

// Ref namespaces of `rev-list --branches`, `--remotes` and `--tags`.
const (
	headsNS   = "refs/heads/"
	remotesNS = "refs/remotes/"
	tagsNS    = "refs/tags/"
)

var allNamespaces = []string{headsNS, remotesNS, tagsNS}

// ref is one head, remote-tracking ref or tag.
type ref struct {
	name   string // full refname
	commit string // what it points at; for annotated tags, the tagged commit
	symref string // target, for a symref like refs/remotes/origin/HEAD
}

// listRefs reads every head, remote-tracking ref and tag (5k refs with tags
// peeled take ~70ms).
func listRefs(ctx context.Context, common string) ([]ref, error) {
	out, err := git.Run(ctx, common, "for-each-ref",
		"--format=%(refname)%00%(objectname)%00%(*objectname)%00%(symref)", "refs/heads", "refs/remotes", "refs/tags")
	if err != nil {
		return nil, err
	}
	var refs []ref
	for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
		if parts := strings.Split(line, "\x00"); len(parts) == 4 {
			refs = append(refs, ref{name: parts[0], commit: cmp.Or(parts[2], parts[1]), symref: parts[3]})
		}
	}
	return refs, nil
}

// refsAt maps each commit to the refs pointing at it, symrefs left out.
func refsAt(refs []ref) map[string][]string {
	at := map[string][]string{}
	for _, r := range refs {
		if r.symref == "" {
			at[r.commit] = append(at[r.commit], r.name)
		}
	}
	return at
}

// lazyRefsAt returns a loader for the commit -> refs map that pointingRef
// uses, reading the refs on first use: a Rescan then gets a fresh read, and
// only pays for it when it needs one.
func lazyRefsAt(ctx context.Context, common string) func() (map[string][]string, error) {
	return sync.OnceValues(func() (map[string][]string, error) {
		refs, err := listRefs(ctx, common)
		return refsAt(refs), err
	})
}

// pointingRef returns a ref in namespaces (eg. headsNS) pointing exactly at
// tip, other than the excluded branches; the first by refname, or "". An
// unreadable ref list just finds nothing.
func (r *Repo) pointingRef(tip string, exclude []string, namespaces ...string) string {
	if r.refsAt == nil {
		return ""
	}
	refsAt, err := r.refsAt()
	if err != nil {
		return ""
	}
	for _, ref := range refsAt[tip] {
		inNamespace := slices.ContainsFunc(namespaces, func(ns string) bool { return strings.HasPrefix(ref, ns) })
		if inNamespace && !excludedBranch(ref, exclude) {
			return ref
		}
	}
	return ""
}

// defaultContaining returns the first default branch (other than the
// excluded branches) whose current value contains tip, or "". Errors count
// as not containing.
func (r *Repo) defaultContaining(ctx context.Context, dir, tip string, exclude []string) string {
	for _, ref := range r.defaultBranches {
		if excludedBranch(ref, exclude) {
			continue
		}
		if ok, err := git.IsAncestor(ctx, dir, tip, ref); err == nil && ok {
			return ref
		}
	}
	return ""
}

func excludedBranch(ref string, exclude []string) bool {
	name, ok := strings.CutPrefix(ref, headsNS)
	return ok && slices.Contains(exclude, name)
}

// countUnreachable is git.CountUnreachable minus the full-ref walk when tip
// provably has nothing unreachable: a surviving ref points exactly at it,
// or a default branch contains it. Either way rev-list would count 0. The
// ref list comes from r's lazily loaded snapshot.
func countUnreachable(ctx context.Context, r *Repo, dir, tip string, exclude []string) (int, error) {
	if r.pointingRef(tip, exclude, allNamespaces...) != "" || r.defaultContaining(ctx, dir, tip, exclude) != "" {
		return 0, nil
	}
	return git.CountUnreachable(ctx, dir, tip, exclude)
}

// CountUnreachableNow is git.CountUnreachable for execute-time safety
// checks: its fast paths read the refs fresh (`for-each-ref --points-at`,
// and each default branch's current value) instead of using a snapshot.
func CountUnreachableNow(ctx context.Context, r *Repo, tip string, exclude []string) (int, error) {
	dir := r.CommonDir
	if r.defaultContaining(ctx, dir, tip, exclude) != "" {
		return 0, nil
	}
	out, err := git.Run(ctx, dir, "for-each-ref", "--points-at", tip, "--format=%(refname)%00%(symref)", "refs/heads", "refs/remotes", "refs/tags")
	if err == nil {
		for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
			ref, symref, _ := strings.Cut(line, "\x00")
			if ref != "" && symref == "" && !excludedBranch(ref, exclude) {
				return 0, nil
			}
		}
	}
	return git.CountUnreachable(ctx, dir, tip, exclude)
}
