package scan

import (
	"context"
	"fmt"
	"slices"
	"strconv"
	"strings"

	"wtprune/git"
)

// authoredPrefixes mark reflog entries that created commits on the branch
// here, as opposed to just moving it to existing commits. "pull" entries
// are handled separately: only a fast-forward creates nothing.
var authoredPrefixes = []string{"commit", "merge", "rebase", "cherry-pick", "revert", "am"}

// Squash detection limits: beyond these the branch is just kept.
const (
	maxSquashRange = 3000 // commits in merge-base..default
	maxSquashFiles = 1000 // files changed on the branch
)

// classifyBranch decides whether the worktree's branch is a deletion
// candidate and, if so, its tier and the ref that contains its tip.
func classifyBranch(ctx context.Context, w *Worktree) error {
	b := &w.Branch
	b.Tip = w.Head
	repo := w.Repo
	switch {
	case w.Detached || b.Name == "":
		b.KeepReason = keepDetached
		return nil
	case b.Tip == "":
		b.KeepReason = "no commits"
		return nil
	case w.InProgress != "":
		b.KeepReason = "in progress"
		return nil
	case repo.protected[b.Name]:
		b.KeepReason = "protected"
		return nil
	case checkedOutElsewhere(w):
		b.KeepReason = "in use elsewhere"
		return nil
	}

	// A default branch containing the tip settles both the unique count (0)
	// and the tier, so check it once, before any full-ref walk.
	exclude := []string{b.Name}
	merged := repo.defaultContaining(ctx, repo.CommonDir, b.Tip, exclude)
	if merged == "" && repo.pointingRef(b.Tip, exclude, allNamespaces...) == "" {
		unique, err := git.CountUnreachable(ctx, repo.CommonDir, b.Tip, exclude)
		if err != nil {
			return err
		}
		if unique > 0 {
			b.KeepReason = fmt.Sprintf("%d unique", unique)
			return squashed(ctx, repo, b)
		}
	}

	tier, ref, sameTip, err := containment(ctx, repo, b, merged)
	if err != nil {
		return err
	}
	if ref == "" {
		// rev-list said contained, so some ref must contain it; be conservative.
		b.KeepReason = "no containing ref found"
		return nil
	}
	b.ContainedIn, b.SameTip = shortRef(ref), sameTip
	if tier != TierPointer {
		b.Tier = tier
		return nil
	}
	authored, err := authoredHere(ctx, w)
	if err != nil {
		return err
	}
	if !authored {
		b.Tier = TierPointer
		return nil
	}
	b.Tier = TierPushed
	return squashed(ctx, repo, b)
}

// squashed upgrades b to TierSquashed when its changes were squash-merged
// into a default branch. Detection failures just leave b as it is.
func squashed(ctx context.Context, repo *Repo, b *Branch) error {
	for _, ref := range repo.defaultBranches {
		if commit, err := squashCommit(ctx, repo.CommonDir, b.Tip, ref); err == nil && commit != "" {
			b.Tier, b.KeepReason = TierSquashed, ""
			b.ContainedIn, b.SameTip = shortRef(ref), false
			b.SquashCommit, b.SquashRef = commit, ref
			return nil
		}
	}
	return nil
}

// squashCommit finds a non-merge commit in merge-base..ref whose patch-id
// equals the whole diff merge-base..tip, and whose files have the same
// content as at tip: the branch squash-merged into ref. patch-id is
// --verbatim (--stable ignores whitespace) and ignores hunk positions, hence
// the content check. An empty branch diff never matches.
func squashCommit(ctx context.Context, dir, tip, ref string) (string, error) {
	mbOut, err := git.Run(ctx, dir, "merge-base", ref, tip)
	if git.ExitCode(err) == 1 {
		return "", nil // no common history
	}
	if err != nil {
		return "", err
	}
	mb := strings.TrimSpace(mbOut)

	files, err := changedFiles(ctx, dir, mb, tip)
	if err != nil || len(files) == 0 || len(files) > maxSquashFiles {
		return "", err
	}
	count, err := git.Run(ctx, dir, "rev-list", "--count", mb+".."+ref)
	if err != nil {
		return "", err
	}
	if n, _ := strconv.Atoi(strings.TrimSpace(count)); n == 0 || n > maxSquashRange {
		return "", nil
	}

	args := append([]string{"--literal-pathspecs", "rev-list", "--no-merges", mb + ".." + ref, "--"}, files...)
	touching, err := git.Run(ctx, dir, args...)
	if err != nil || strings.TrimSpace(touching) == "" {
		return "", err
	}
	sameFiles, err := commitsChangingExactly(ctx, dir, strings.Fields(touching), files)
	if err != nil || len(sameFiles) == 0 {
		return "", err
	}

	branchDiff, err := git.Run(ctx, dir, "diff-tree", "-p", "--no-renames", mb, tip)
	if err != nil {
		return "", err
	}
	branchID, err := git.RunInput(ctx, dir, branchDiff, "patch-id", "--verbatim")
	if err != nil {
		return "", err
	}
	want, _, _ := strings.Cut(strings.TrimSpace(branchID), " ")
	if want == "" {
		return "", nil
	}
	diffs, err := git.RunInput(ctx, dir, strings.Join(sameFiles, "\n")+"\n", "diff-tree", "-p", "--no-renames", "--stdin")
	if err != nil {
		return "", err
	}
	ids, err := git.RunInput(ctx, dir, diffs, "patch-id", "--verbatim")
	if err != nil {
		return "", err
	}
	for _, line := range strings.Split(strings.TrimSpace(ids), "\n") {
		id, commit, ok := strings.Cut(line, " ")
		if !ok || id != want {
			continue
		}
		if same, err := sameContent(ctx, dir, commit, tip, files); err != nil || same {
			return commit, err
		}
	}
	return "", nil
}

// SquashStillHolds re-checks a squash-merge verdict: the squash commit is
// still in ref, and every file it changed has the same content at tip.
func SquashStillHolds(ctx context.Context, dir, tip, squash, ref string) (bool, error) {
	if ok, err := git.IsAncestor(ctx, dir, squash, ref); err != nil || !ok {
		return false, err
	}
	files, err := changedFiles(ctx, dir, squash+"^", squash)
	if err != nil || len(files) == 0 {
		return false, err
	}
	return sameContent(ctx, dir, squash, tip, files)
}

// sameContent reports whether files are identical in commits a and b.
func sameContent(ctx context.Context, dir, a, b string, files []string) (bool, error) {
	args := append([]string{"--literal-pathspecs", "diff-tree", "--quiet", "-r", "--no-renames", a, b, "--"}, files...)
	_, err := git.Run(ctx, dir, args...)
	if git.ExitCode(err) == 1 {
		return false, nil
	}
	return err == nil, err
}

func changedFiles(ctx context.Context, dir, from, to string) ([]string, error) {
	out, err := git.Run(ctx, dir, "diff-tree", "-r", "--name-only", "-z", "--no-renames", from, to)
	if err != nil {
		return nil, err
	}
	files := strings.Split(strings.TrimSuffix(out, "\x00"), "\x00")
	if len(files) == 1 && files[0] == "" {
		return nil, nil
	}
	slices.Sort(files)
	return files, nil
}

// commitsChangingExactly keeps the commits whose own diff touches exactly
// files (sorted): a squash of the branch can't touch anything else.
func commitsChangingExactly(ctx context.Context, dir string, commits, files []string) ([]string, error) {
	out, err := git.RunInput(ctx, dir, strings.Join(commits, "\n")+"\n", "diff-tree", "-r", "--name-only", "-z", "--no-renames", "--stdin")
	if err != nil {
		return nil, err
	}
	touched := map[string][]string{}
	var cur string
	for _, field := range strings.Split(out, "\x00") {
		if slices.Contains(commits, field) {
			cur = field
			continue
		}
		if cur != "" && field != "" {
			touched[cur] = append(touched[cur], field)
		}
	}
	var same []string
	for _, c := range commits {
		got := touched[c]
		slices.Sort(got)
		if slices.Equal(got, files) {
			same = append(same, c)
		}
	}
	return same, nil
}

// containment finds one ref containing the tip of a candidate branch, and
// the tier that implies: merged (merged is the default branch containing
// it, if any), contained (another local branch or a tag) or pointer (only
// remote-tracking refs; the caller splits that into pointer/pushed). Cheap
// checks first: ref scans with --contains are slow in repos with thousands
// of refs.
func containment(ctx context.Context, repo *Repo, b *Branch, merged string) (tier Tier, ref string, sameTip bool, err error) {
	dir := repo.CommonDir
	if merged != "" {
		return TierMerged, merged, false, nil
	}
	if ref := repo.pointingRef(b.Tip, []string{b.Name}, headsNS, tagsNS); ref != "" {
		return TierContained, ref, true, nil
	}
	out, err := git.Run(ctx, dir, "rev-list", "--count", b.Tip, "--not", "--exclude="+b.Name, "--branches", "--tags")
	if err != nil {
		return TierNone, "", false, err
	}
	if strings.TrimSpace(out) == "0" {
		c, err := firstContainingRef(ctx, dir, b.Tip, "--exclude=refs/heads/"+b.Name, "refs/heads", "refs/tags")
		return TierContained, c.ref, c.sha == b.Tip, err
	}
	if b.Upstream != "" {
		upstream := "refs/remotes/" + b.Upstream
		if ok, err := git.IsAncestor(ctx, dir, b.Tip, upstream); err == nil && ok {
			sha, err := git.Run(ctx, dir, "rev-parse", "--verify", upstream)
			return TierPointer, upstream, strings.TrimSpace(sha) == b.Tip, err
		}
	}
	c, err := firstContainingRef(ctx, dir, b.Tip, "--exclude=refs/remotes/*/HEAD", "refs/remotes")
	return TierPointer, c.ref, c.sha == b.Tip, err
}

// checkedOutElsewhere reports whether another worktree has the branch
// checked out (git refuses to delete it then).
func checkedOutElsewhere(w *Worktree) bool {
	for _, reg := range w.Repo.registered {
		if reg.entry.Branch == "refs/heads/"+w.Branch.Name && reg.adminDir != w.AdminDir {
			return true
		}
	}
	return false
}

type containingRef struct {
	ref string
	sha string // commit the ref points at (peeled for annotated tags)
}

// firstContainingRef returns one ref matching the for-each-ref args that
// contains tip, or a zero value. Symrefs (eg. origin/HEAD) are skipped.
func firstContainingRef(ctx context.Context, dir, tip string, args ...string) (containingRef, error) {
	full := []string{"for-each-ref", "--contains", tip, "--count=1", "--format=%(refname)%00%(objectname)%00%(*objectname)%00%(symref)"}
	out, err := git.Run(ctx, dir, append(full, args...)...)
	if err != nil {
		return containingRef{}, err
	}
	parts := strings.Split(strings.TrimSpace(out), "\x00")
	if len(parts) != 4 || parts[3] != "" {
		return containingRef{}, nil
	}
	sha := parts[1]
	if parts[2] != "" {
		sha = parts[2]
	}
	return containingRef{ref: parts[0], sha: sha}, nil
}

// authoredHere reports whether the branch has commits made here: its reflog
// shows commits/merges/rebases, or a commit not in a default branch or tag
// has user.email as author or committer. A missing reflog counts as
// authored: we can't prove it's a pointer.
func authoredHere(ctx context.Context, w *Worktree) (bool, error) {
	b, repo := w.Branch, w.Repo
	var msgs []string
	if entries, ok := w.readReflog("refs/heads/" + b.Name); ok {
		for _, e := range entries {
			msgs = append(msgs, e.msg)
		}
	} else {
		out, err := git.Run(ctx, repo.CommonDir, "reflog", "show", "--format=%gs", "refs/heads/"+b.Name, "--")
		if err != nil {
			return false, err
		}
		if out = strings.TrimSpace(out); out != "" {
			msgs = strings.Split(out, "\n")
		}
	}
	if len(msgs) == 0 {
		return true, nil
	}
	for _, msg := range msgs {
		if authoredEntry(msg) {
			return true, nil
		}
	}

	email, err := git.Run(ctx, w.Path, "config", "--get", "user.email")
	email = strings.TrimSpace(email)
	if git.ExitCode(err) == 1 || email == "" {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	args := []string{"log", "--format=%ae%n%ce", b.Tip, "--not", "--tags"}
	out, err := git.Run(ctx, repo.CommonDir, append(append(args, repo.defaultBranches...), "--")...)
	if err != nil {
		return false, err
	}
	for _, e := range strings.Fields(out) {
		if strings.EqualFold(e, email) {
			return true, nil
		}
	}
	return false, nil
}

func authoredEntry(msg string) bool {
	if strings.HasPrefix(msg, "pull") {
		return !strings.HasSuffix(msg, ": Fast-forward")
	}
	for _, p := range authoredPrefixes {
		if strings.HasPrefix(msg, p) {
			return true
		}
	}
	return false
}
