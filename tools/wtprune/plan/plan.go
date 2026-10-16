// Package plan turns a selection of scanned worktrees into a reviewable
// Plan, and executes it with execute-time safety checks.
package plan

import (
	"context"
	"fmt"
	"slices"
	"strings"

	"wtprune/git"
	"wtprune/internal/text"
	"wtprune/scan"
)

// Selection is what the user picked: worktree IDs to remove, and worktree
// IDs whose branch should be deleted too.
type Selection struct {
	Worktrees []string
	Branches  []string
}

// Item is one worktree removal.
type Item struct {
	W            *scan.Worktree
	Trash        bool // moved to the Trash first: at risk or has ignored files
	Force        bool // --no-trash: `worktree remove --force` instead
	DeleteBranch bool
}

// Reasons lists what removal would lose, one line per kind, for the review
// screen, eg. "DIRTY: M go.mod, ?? notes.md" or "+1 ignored file: .env".
func (it Item) Reasons() []string {
	w := it.W
	var lines []string
	if w.Dirty() {
		var files []string
		for _, c := range w.Changes {
			files = append(files, c.String())
		}
		lines = append(lines, "DIRTY: "+strings.Join(files, ", "))
	}
	if w.DetachedLost > 0 {
		lines = append(lines, fmt.Sprintf("DETACHED +%d LOST (only this HEAD has them)", w.DetachedLost))
	}
	if w.InProgress != "" {
		lines = append(lines, w.InProgress)
	}
	if len(w.Hidden) > 0 {
		lines = append(lines, "HIDDEN (assume-unchanged): "+strings.Join(w.Hidden, ", "))
	}
	if len(w.InUse) > 0 {
		var procs []string
		for _, p := range w.InUse {
			procs = append(procs, fmt.Sprintf("%s (pid %d)", p.Command, p.PID))
		}
		lines = append(lines, "IN USE: "+strings.Join(procs, ", "))
	}
	if n := w.IgnoredCount(); n > 0 {
		lines = append(lines, fmt.Sprintf("+%d ignored: %s", n, strings.Join(slices.Concat(w.IgnoredFiles, w.IgnoredDirs), ", ")))
	}
	return lines
}

// BranchAction is one planned `git branch -D`.
type BranchAction struct {
	Repo       *scan.Repo
	Name       string
	Tip        string // at scan time; deletion requires it unchanged
	Tier       scan.Tier
	Label      string // eg. "pointer (= origin/feat-x)"
	WorktreeID string

	SquashCommit string // TierSquashed: must still be in SquashRef at execute time
	SquashRef    string
}

// NeedsTypedYes reports whether this branch alone requires typing "yes".
func (b BranchAction) NeedsTypedYes() bool { return b.Tier == scan.TierPushed }

// Dropped is a selected worktree or branch that the plan leaves out.
type Dropped struct {
	Kind   string // "worktree" or "branch"
	Name   string // path or branch name
	Reason string
}

// Plan is everything Execute will do, grouped for the review screen.
type Plan struct {
	AtRisk   []Item
	Safe     []Item
	Branches []BranchAction
	Dropped  []Dropped
	NoTrash  bool
}

// Build plans the selection against the scanned rows. Blocked or unknown
// worktrees, and branches that aren't candidates or whose worktree isn't
// removed, are dropped rather than acted on.
func Build(rows []*scan.Worktree, sel Selection, noTrash bool) *Plan {
	p := &Plan{NoTrash: noTrash}
	byID := map[string]*scan.Worktree{}
	for _, w := range rows {
		byID[w.ID] = w
	}

	planned := map[string]bool{}
	for _, id := range dedupe(sel.Worktrees) {
		w, ok := byID[id]
		switch {
		case !ok:
			p.Dropped = append(p.Dropped, Dropped{Kind: "worktree", Name: id, Reason: "not in scan"})
			continue
		case !w.Selectable():
			p.Dropped = append(p.Dropped, Dropped{Kind: "worktree", Name: w.Path, Reason: "blocked: " + string(w.State)})
			continue
		}
		planned[id] = true
	}

	for _, id := range dedupe(sel.Branches) {
		w, ok := byID[id]
		switch {
		case !ok:
			p.Dropped = append(p.Dropped, Dropped{Kind: "branch", Name: id, Reason: "worktree not in scan"})
		case !planned[id]:
			p.Dropped = append(p.Dropped, Dropped{Kind: "branch", Name: branchName(w), Reason: "its worktree is not being removed"})
		case !w.Branch.Candidate():
			p.Dropped = append(p.Dropped, Dropped{Kind: "branch", Name: branchName(w), Reason: "keep: " + w.Branch.Label()})
		}
	}
	// Scan order (oldest idle first), not selection order.
	var branches []BranchAction
	for _, w := range rows {
		if planned[w.ID] && slices.Contains(sel.Branches, w.ID) && w.Branch.Candidate() {
			branches = append(branches, BranchAction{
				Repo: w.Repo, Name: w.Branch.Name, Tip: w.Branch.Tip, Tier: w.Branch.Tier,
				Label: w.Branch.Label(), WorktreeID: w.ID,
				SquashCommit: w.Branch.SquashCommit, SquashRef: w.Branch.SquashRef,
			})
		}
	}
	p.Branches, p.Dropped = dropBatchLosses(branches, p.Dropped)

	doomed := map[string][]string{} // by CommonDir
	for _, b := range p.Branches {
		doomed[b.Repo.CommonDir] = append(doomed[b.Repo.CommonDir], b.Name)
	}
	for _, w := range rows {
		if !planned[w.ID] {
			continue
		}
		if reason := lostToBranchDeletions(w, doomed[w.Repo.CommonDir]); reason != "" {
			p.Dropped = append(p.Dropped, Dropped{Kind: "worktree", Name: w.Path, Reason: reason})
			continue
		}
		deleting := slices.ContainsFunc(p.Branches, func(b BranchAction) bool { return b.WorktreeID == w.ID })
		it := Item{W: w, DeleteBranch: deleting}
		if w.NeedsTrash() {
			it.Trash = !noTrash
			it.Force = noTrash && w.AtRisk()
		}
		if w.AtRisk() {
			p.AtRisk = append(p.AtRisk, it)
		} else {
			p.Safe = append(p.Safe, it)
		}
	}
	return p
}

// dropBatchLosses drops branches whose commits are contained only in other
// branches of the same batch (eg. two branches at the same unique commit):
// deleting them all would lose those commits. Execute re-checks this too.
// Squash-merged branches are exempt: their content is in a default branch.
func dropBatchLosses(branches []BranchAction, dropped []Dropped) ([]BranchAction, []Dropped) {
	keep := slices.Clone(branches)
	for _, b := range branches {
		exclude := []string{b.Name}
		for _, other := range keep {
			if other.WorktreeID != b.WorktreeID && other.Repo.CommonDir == b.Repo.CommonDir {
				exclude = append(exclude, other.Name)
			}
		}
		if b.Tier == scan.TierSquashed || len(exclude) == 1 {
			continue // alone in its repo: the scan already checked it
		}
		reason := ""
		n, err := scan.CountUnreachableNow(context.Background(), b.Repo, b.Tip, exclude)
		switch {
		case err != nil:
			reason = "could not check the other planned deletions: " + err.Error()
		case n > 0:
			reason = fmt.Sprintf("only other ref is also being deleted (would lose %d)", n)
		}
		if reason != "" {
			dropped = append(dropped, Dropped{Kind: "branch", Name: b.Name, Reason: reason})
			keep = slices.DeleteFunc(keep, func(k BranchAction) bool { return k.WorktreeID == b.WorktreeID })
		}
	}
	return keep, dropped
}

// lostToBranchDeletions explains why removing w must wait until the
// planned branch deletions are done, or returns "". A failed check keeps w
// in the plan: Execute checks again and does not remove it if that fails.
func lostToBranchDeletions(w *scan.Worktree, doomed []string) string {
	if _, more, heldBy, err := lostWith(context.Background(), w, doomed); err == nil && more > 0 {
		return heldOnlyBy(more, heldBy) + "; unflag " + text.Plural(len(heldBy), "it", "them") + " (b), or remove this worktree next run"
	}
	return ""
}

// heldOnlyBy says that n commits of a worktree are only in the branches
// heldBy, which are being deleted.
func heldOnlyBy(n int, heldBy []string) string {
	commits := fmt.Sprintf("%d %s", n, text.Plural(n, "commit", "commits"))
	if len(heldBy) == 0 {
		return "holds " + commits + " that otherwise only branches being deleted have"
	}
	return fmt.Sprintf("holds %s only %s %s (being deleted)", commits, strings.Join(heldBy, ", "), text.Plural(len(heldBy), "has", "have"))
}

// lostWith lists the commits that removing the worktree would leave
// unreachable (its detached HEAD, any in-progress operation's heads), and
// counts how many more it would once the branches in doomed are deleted:
// the scan counted those as safe in a branch, eg. a detached HEAD at a
// commit of a squash-merged branch that is being deleted. heldBy names the
// doomed branches holding those.
func lostWith(ctx context.Context, w *scan.Worktree, doomed []string) (lost []string, more int, heldBy []string, err error) {
	if !w.Detached && w.InProgress == "" {
		return nil, 0, nil, nil
	}
	// Only when the scan saw something lost: recomputing it here instead
	// would count a commit as lost, not as "more", if a doomed branch
	// holding it were deleted in between, and so let the removal through.
	if w.DetachedLost > 0 || w.InProgress != "" {
		if lost, err = scan.LostCommits(ctx, w, nil); err != nil {
			return nil, 0, nil, err
		}
	}
	if w.InProgress == "" && len(lost) > w.DetachedLost {
		// A ref holding some of them went away since w was inspected.
		return lost, len(lost) - w.DetachedLost, nil, nil
	}
	if len(doomed) == 0 {
		return lost, 0, nil, nil
	}
	withoutDoomed, err := scan.LostCommits(ctx, w, doomed)
	if err != nil {
		return nil, 0, nil, err
	}
	extra := slices.DeleteFunc(withoutDoomed, func(c string) bool { return slices.Contains(lost, c) })
	if len(extra) == 0 {
		return lost, 0, nil, nil
	}
	return lost, len(extra), branchesContaining(ctx, w.Repo.CommonDir, extra, doomed), nil
}

// branchesContaining names the branches (of names) that contain any of
// commits. Errors just name none.
func branchesContaining(ctx context.Context, dir string, commits, names []string) []string {
	args := []string{"for-each-ref", "--format=%(refname:lstrip=2)"}
	for _, c := range commits {
		args = append(args, "--contains", c)
	}
	for _, name := range names {
		args = append(args, "refs/heads/"+name)
	}
	out, err := git.Run(ctx, dir, args...)
	if err != nil {
		return nil
	}
	return strings.Fields(out)
}

// Items is every removal in execution order: at risk first, then safe.
func (p *Plan) Items() []Item { return slices.Concat(p.AtRisk, p.Safe) }

// Empty reports whether there is nothing to do.
func (p *Plan) Empty() bool { return len(p.AtRisk)+len(p.Safe) == 0 }

// NeedsTypedYes reports whether confirming requires typing "yes" rather
// than "y": any at-risk worktree or any pushed branch.
func (p *Plan) NeedsTypedYes() bool {
	if len(p.AtRisk) > 0 {
		return true
	}
	return slices.ContainsFunc(p.Branches, BranchAction.NeedsTypedYes)
}

func branchName(w *scan.Worktree) string {
	if w.Branch.Name == "" {
		return "(detached) in " + w.Path
	}
	return w.Branch.Name
}

func dedupe(ids []string) []string {
	var out []string
	for _, id := range ids {
		if !slices.Contains(out, id) {
			out = append(out, id)
		}
	}
	return out
}
