package tui_test

import (
	"slices"
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/x/ansi"

	"wtprune/scan"
	"wtprune/tui"
)

var testRepo = &scan.Repo{Name: "repo", CommonDir: "/src/repo/.git", MainPath: "/src/repo"}

// wt is a SAFE row whose branch is kept (not a deletion candidate).
func wt(name string) *scan.Worktree {
	return &scan.Worktree{
		ID:     "/wt/" + name,
		Path:   "/wt/" + name,
		Repo:   testRepo,
		Branch: scan.Branch{Name: name, KeepReason: "1 unique"},
	}
}

func key(s string) tea.KeyMsg { return tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(s)} }

var (
	space     = tea.KeyMsg{Type: tea.KeySpace, Runes: []rune{' '}}
	enter     = tea.KeyMsg{Type: tea.KeyEnter}
	esc       = tea.KeyMsg{Type: tea.KeyEsc}
	up        = tea.KeyMsg{Type: tea.KeyUp}
	down      = tea.KeyMsg{Type: tea.KeyDown}
	backspace = tea.KeyMsg{Type: tea.KeyBackspace}
	ctrlC     = tea.KeyMsg{Type: tea.KeyCtrlC}
)

// send delivers msgs in order and returns the last command.
func send(m *tui.Model, msgs ...tea.Msg) tea.Cmd {
	var cmd tea.Cmd
	for _, msg := range msgs {
		_, cmd = m.Update(msg)
	}
	return cmd
}

func isQuit(cmd tea.Cmd) bool {
	if cmd == nil {
		return false
	}
	_, ok := cmd().(tea.QuitMsg)
	return ok
}

func assertIDs(t *testing.T, what string, got, want []string) {
	t.Helper()
	if !slices.Equal(got, want) {
		t.Errorf("%s = %v, want %v", what, got, want)
	}
}

func TestSpaceTogglesCursorRowAndMovesDown(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a"), wt("b")}})

	send(m, space)
	assertIDs(t, "after space", m.Selection().Worktrees, []string{"/wt/a"})

	send(m, space)
	assertIDs(t, "after second space", m.Selection().Worktrees, []string{"/wt/a", "/wt/b"})

	send(m, space)
	assertIDs(t, "after space on last row", m.Selection().Worktrees, []string{"/wt/a"})

	send(m, up, space)
	assertIDs(t, "after up space", m.Selection().Worktrees, nil)
}

func TestSpaceMovesDownPastBlockedRow(t *testing.T) {
	locked := wt("locked")
	locked.State = scan.StateLocked
	m := tui.New(tui.Config{Rows: []*scan.Worktree{locked, wt("ok")}})

	send(m, space, space)
	assertIDs(t, "after space twice", m.Selection().Worktrees, []string{"/wt/ok"})
}

func TestBlockedRowsCannotBeSelected(t *testing.T) {
	locked := wt("locked")
	locked.State = scan.StateLocked
	locked.Hint = "git worktree unlock /wt/locked"
	locked.Branch = scan.Branch{Name: "locked", Tier: scan.TierMerged, ContainedIn: "main"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{locked, wt("ok")}})

	send(m, space)
	assertIDs(t, "after space on locked", m.Selection().Worktrees, nil)
	if !strings.Contains(m.Hint(), "git worktree unlock /wt/locked") {
		t.Errorf("hint = %q, want the unlock hint", m.Hint())
	}

	send(m, key("b"))
	assertIDs(t, "after b on locked", m.Selection().Worktrees, nil)

	send(m, key("a"))
	assertIDs(t, "after a", m.Selection().Worktrees, []string{"/wt/ok"})

	send(m, key("c"))
	assertIDs(t, "after c", m.Selection().Worktrees, []string{"/wt/ok"})
}

func TestBlockedRowDetailShowsHint(t *testing.T) {
	locked := wt("locked")
	locked.State = scan.StateLocked
	locked.Hint = "git worktree unlock /wt/locked"
	m := tui.New(tui.Config{Rows: []*scan.Worktree{locked}})

	if view := m.View(); !strings.Contains(view, "git worktree unlock /wt/locked") {
		t.Errorf("view does not show the blocked hint:\n%s", view)
	}
}

func TestSelectAllAndNone(t *testing.T) {
	dirty := wt("dirty")
	dirty.Modified = 1
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a"), dirty}})

	send(m, key("a"))
	assertIDs(t, "after a", m.Selection().Worktrees, []string{"/wt/a", "/wt/dirty"})

	send(m, key("n"))
	assertIDs(t, "after n", m.Selection().Worktrees, nil)
}

func TestBulkSelectActsOnFilteredRowsOnly(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("foo-1"), wt("bar"), wt("foo-2")}})

	send(m, key("/"), key("foo"), enter, key("a"))
	assertIDs(t, "after a on filter foo", m.Selection().Worktrees, []string{"/wt/foo-1", "/wt/foo-2"})

	send(m, esc, key("a"))
	assertIDs(t, "after clearing the filter and a", m.Selection().Worktrees, []string{"/wt/foo-1", "/wt/bar", "/wt/foo-2"})
}

func TestSafeOnlyActsOnFilteredRowsOnly(t *testing.T) {
	fooDirty := wt("foo-dirty")
	fooDirty.Modified = 1
	barDirty := wt("bar-dirty")
	barDirty.Untracked = 1
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("foo-safe"), fooDirty, barDirty}})

	send(m, key("a"), key("/"), key("foo"), enter, key("c"))
	assertIDs(t, "after c on filter foo", m.Selection().Worktrees, []string{"/wt/foo-safe", "/wt/bar-dirty"})
}

func TestBulkBranchesActOnFilteredSelectedRowsOnly(t *testing.T) {
	fooMerged := wt("foo-merged")
	fooMerged.Branch = scan.Branch{Name: "foo-merged", Tier: scan.TierMerged, ContainedIn: "main"}
	barMerged := wt("bar-merged")
	barMerged.Branch = scan.Branch{Name: "bar-merged", Tier: scan.TierMerged, ContainedIn: "main"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{fooMerged, barMerged}})

	send(m, key("a"), key("/"), key("foo"), enter, key("B"))
	assertIDs(t, "branches after B on filter foo", m.Selection().Branches, []string{"/wt/foo-merged"})
}

func TestFilterTypingDoesNotTriggerKeys(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a"), wt("b")}})

	send(m, key("/"), key("a"), key("n"))
	assertIDs(t, "while typing a filter", m.Selection().Worktrees, nil)

	send(m, backspace, backspace, esc, key("a"))
	assertIDs(t, "after esc and a", m.Selection().Worktrees, []string{"/wt/a", "/wt/b"})
}

func TestEscClearsFilter(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("foo"), wt("bar")}})

	send(m, key("/"), key("foo"), enter)
	if view := m.View(); strings.Contains(view, "bar") {
		t.Fatalf("filtered view still shows bar:\n%s", view)
	}

	send(m, esc)
	if view := m.View(); !strings.Contains(view, "bar") {
		t.Errorf("view after esc does not show bar:\n%s", view)
	}
}

func TestBranchKeySelectsRowAndDeselectClearsFlag(t *testing.T) {
	merged := wt("merged")
	merged.Branch = scan.Branch{Name: "merged", Tier: scan.TierMerged, ContainedIn: "main"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged}})

	send(m, key("b"))
	assertIDs(t, "worktrees after b", m.Selection().Worktrees, []string{"/wt/merged"})
	assertIDs(t, "branches after b", m.Selection().Branches, []string{"/wt/merged"})

	send(m, key("b"))
	assertIDs(t, "worktrees after b off", m.Selection().Worktrees, []string{"/wt/merged"})
	assertIDs(t, "branches after b off", m.Selection().Branches, nil)

	send(m, key("b"), space)
	assertIDs(t, "worktrees after deselecting", m.Selection().Worktrees, nil)
	send(m, space)
	assertIDs(t, "branches after reselecting", m.Selection().Branches, nil)
}

func TestAutoBranchesFlagsBulkRowsAsTheyAreSelected(t *testing.T) {
	merged := wt("merged")
	merged.Branch = scan.Branch{Name: "merged", Tier: scan.TierMerged, ContainedIn: "main"}
	pushed := wt("pushed")
	pushed.Branch = scan.Branch{Name: "pushed", Tier: scan.TierPushed, ContainedIn: "origin/pushed"}
	squashed := wt("squashed")
	squashed.Branch = scan.Branch{Name: "squashed", Tier: scan.TierSquashed, ContainedIn: "origin/main"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged, pushed, squashed, wt("kept")}})

	send(m, key("B"))
	assertIDs(t, "branches after B with nothing selected", m.Selection().Branches, nil)
	if view := m.View(); !strings.Contains(view, "(B: auto)") {
		t.Errorf("header does not show auto branch deletion:\n%s", view)
	}

	send(m, space)
	assertIDs(t, "branches after selecting merged", m.Selection().Branches, []string{"/wt/merged"})

	send(m, key("a"))
	assertIDs(t, "branches after a", m.Selection().Branches, []string{"/wt/merged", "/wt/squashed"})
}

func TestAutoBranchesOnFlagsSelectedRowsAndOffClearsAll(t *testing.T) {
	merged := wt("merged")
	merged.Branch = scan.Branch{Name: "merged", Tier: scan.TierMerged, ContainedIn: "main"}
	pushed := wt("pushed")
	pushed.Branch = scan.Branch{Name: "pushed", Tier: scan.TierPushed, ContainedIn: "origin/pushed"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged, pushed}})

	send(m, space, down, key("b"))
	assertIDs(t, "branches after space and b on pushed", m.Selection().Branches, []string{"/wt/pushed"})

	send(m, key("B"))
	assertIDs(t, "branches after B on", m.Selection().Branches, []string{"/wt/merged", "/wt/pushed"})

	send(m, key("B"))
	assertIDs(t, "branches after B off", m.Selection().Branches, nil)
	assertIDs(t, "worktrees after B off", m.Selection().Worktrees, []string{"/wt/merged", "/wt/pushed"})
}

func TestNoneClearsSelectionAndFlagsOfFilteredOutRows(t *testing.T) {
	foo := wt("foo")
	foo.Branch = scan.Branch{Name: "foo", Tier: scan.TierMerged, ContainedIn: "main"}
	bar := wt("bar")
	bar.Branch = scan.Branch{Name: "bar", Tier: scan.TierMerged, ContainedIn: "main"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{foo, bar}})

	send(m, key("a"), key("B"), key("/"), key("foo"), enter, key("n"), esc)
	assertIDs(t, "worktrees after n", m.Selection().Worktrees, nil)

	send(m, key("B"), key("a"))
	assertIDs(t, "branches after turning B off and reselecting", m.Selection().Branches, nil)
}

func TestSelectCleanWithDeleteBranchesFlagsSafeBulkRows(t *testing.T) {
	merged := wt("merged")
	merged.Branch = scan.Branch{Name: "merged", Tier: scan.TierMerged, ContainedIn: "main"}
	dirtyMerged := wt("dirty-merged")
	dirtyMerged.Branch = scan.Branch{Name: "dirty-merged", Tier: scan.TierMerged, ContainedIn: "main"}
	dirtyMerged.Modified = 1
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged, dirtyMerged}, SelectClean: true, DeleteBranches: true})

	assertIDs(t, "initial worktrees", m.Selection().Worktrees, []string{"/wt/merged"})
	assertIDs(t, "initial branches", m.Selection().Branches, []string{"/wt/merged"})
}

func TestBranchKeyIgnoresNonCandidates(t *testing.T) {
	kept := wt("kept")
	kept.Branch = scan.Branch{Name: "kept", KeepReason: "2 unique"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{kept}})

	send(m, key("b"))
	assertIDs(t, "worktrees after b", m.Selection().Worktrees, nil)
	if !strings.Contains(m.Hint(), "2 unique") {
		t.Errorf("hint = %q, want the keep reason", m.Hint())
	}
}

func TestBulkBranchesSkipPushedTier(t *testing.T) {
	merged := wt("merged")
	merged.Branch = scan.Branch{Name: "merged", Tier: scan.TierMerged, ContainedIn: "main"}
	pointer := wt("pointer")
	pointer.Branch = scan.Branch{Name: "pointer", Tier: scan.TierPointer, ContainedIn: "origin/pointer", SameTip: true}
	pushed := wt("pushed")
	pushed.Branch = scan.Branch{Name: "pushed", Tier: scan.TierPushed, ContainedIn: "origin/pushed"}
	unselected := wt("unselected")
	unselected.Branch = scan.Branch{Name: "unselected", Tier: scan.TierMerged, ContainedIn: "main"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged, pointer, pushed, unselected}})

	send(m, space, space, space, key("B"))
	assertIDs(t, "branches after B", m.Selection().Branches, []string{"/wt/merged", "/wt/pointer"})

	send(m, key("B"))
	assertIDs(t, "branches after B again", m.Selection().Branches, nil)

	send(m, up, key("b"))
	assertIDs(t, "branches after b on pushed", m.Selection().Branches, []string{"/wt/pushed"})
}

func TestDeleteBranchesFlagsBulkBranchesOnSelect(t *testing.T) {
	merged := wt("merged")
	merged.Branch = scan.Branch{Name: "merged", Tier: scan.TierMerged, ContainedIn: "main"}
	pushed := wt("pushed")
	pushed.Branch = scan.Branch{Name: "pushed", Tier: scan.TierPushed, ContainedIn: "origin/pushed"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged, pushed, wt("kept")}, DeleteBranches: true})

	assertIDs(t, "initial worktrees", m.Selection().Worktrees, nil)

	send(m, key("a"))
	assertIDs(t, "branches after a", m.Selection().Branches, []string{"/wt/merged"})
}

func TestSelectCleanStartsWithSafeRows(t *testing.T) {
	dirty := wt("dirty")
	dirty.Modified = 1
	current := wt("current")
	current.State = scan.StateCurrent
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("safe"), dirty, current}, SelectClean: true})

	assertIDs(t, "initial worktrees", m.Selection().Worktrees, []string{"/wt/safe"})
}

func TestConfirmWithYWhenNothingAtRisk(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("safe")}})

	send(m, space, enter)
	if !m.Reviewing() {
		t.Fatal("enter did not open the review screen")
	}
	if cmd := send(m, key("y")); !isQuit(cmd) {
		t.Fatal("y did not quit")
	}
	p := m.ConfirmedPlan()
	if p == nil || len(p.Safe) != 1 {
		t.Fatalf("confirmed plan = %+v, want one safe item", p)
	}
}

func TestAtRiskNeedsTypedYes(t *testing.T) {
	dirty := wt("dirty")
	dirty.Modified = 1
	dirty.Changes = []scan.Change{{Code: "M", Path: "go.mod"}}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{dirty}})

	send(m, space, enter)
	if view := m.View(); !strings.Contains(view, "DIRTY: M go.mod") || !strings.Contains(view, `"yes"`) {
		t.Errorf("review screen lacks the file list or typed-yes prompt:\n%s", view)
	}

	if cmd := send(m, key("y"), enter); isQuit(cmd) || m.ConfirmedPlan() != nil {
		t.Fatal(`"y" + enter confirmed an at-risk plan`)
	}
	if !strings.Contains(m.Hint(), "not confirmed") {
		t.Errorf("hint after wrong text = %q", m.Hint())
	}

	if cmd := send(m, key("ye"), enter); isQuit(cmd) || m.ConfirmedPlan() != nil {
		t.Fatal(`"ye" + enter confirmed an at-risk plan`)
	}

	if cmd := send(m, key("y"), key("e"), key("s"), enter); !isQuit(cmd) {
		t.Fatal(`"yes" + enter did not quit`)
	}
	if p := m.ConfirmedPlan(); p == nil || len(p.AtRisk) != 1 {
		t.Fatalf("confirmed plan = %+v, want one at-risk item", p)
	}
}

func TestPushedBranchNeedsTypedYes(t *testing.T) {
	pushed := wt("pushed")
	pushed.Branch = scan.Branch{Name: "pushed", Tier: scan.TierPushed, ContainedIn: "origin/pushed"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{pushed}})

	send(m, key("b"), enter)
	if cmd := send(m, key("y")); isQuit(cmd) || m.ConfirmedPlan() != nil {
		t.Fatal(`"y" confirmed a plan deleting a pushed branch`)
	}

	send(m, backspace, key("yes"), enter)
	p := m.ConfirmedPlan()
	if p == nil || len(p.Branches) != 1 {
		t.Fatalf("confirmed plan = %+v, want one branch", p)
	}
}

func TestEnterWithEmptyPlanShowsHint(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}})

	send(m, enter)
	if m.Reviewing() {
		t.Fatal("enter opened the review screen with nothing selected")
	}
	if view := m.View(); !strings.Contains(view, "nothing selected") {
		t.Errorf("view lacks the empty-plan hint:\n%s", view)
	}
}

func TestEscLeavesReviewKeepingSelection(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}})

	send(m, space, enter, esc)
	if m.Reviewing() {
		t.Fatal("esc did not leave the review screen")
	}
	assertIDs(t, "selection after esc", m.Selection().Worktrees, []string{"/wt/a"})

	send(m, key("y"))
	if m.ConfirmedPlan() != nil {
		t.Fatal("y on the list screen confirmed")
	}
}

func TestQuitWithoutConfirming(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}})
	if cmd := send(m, space, key("q")); !isQuit(cmd) {
		t.Fatal("q did not quit")
	}
	if m.ConfirmedPlan() != nil {
		t.Fatal("q produced a confirmed plan")
	}

	m = tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}})
	if cmd := send(m, space, enter, ctrlC); !isQuit(cmd) {
		t.Fatal("ctrl+c on the review screen did not quit")
	}
	if m.ConfirmedPlan() != nil {
		t.Fatal("ctrl+c produced a confirmed plan")
	}
}

func TestCursorRowStaysVisible(t *testing.T) {
	var rows []*scan.Worktree
	for _, name := range []string{"r00", "r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08", "r09",
		"r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19"} {
		rows = append(rows, wt(name))
	}
	m := tui.New(tui.Config{Rows: rows})
	send(m, tea.WindowSizeMsg{Width: 100, Height: 14})

	send(m, key("G"))
	if view := m.View(); !strings.Contains(view, "r19") || strings.Contains(view, "r00") {
		t.Errorf("after G the view should show r19 and not r00:\n%s", view)
	}

	send(m, key("g"))
	if view := m.View(); !strings.Contains(view, "r00") || strings.Contains(view, "r19") {
		t.Errorf("after g the view should show r00 and not r19:\n%s", view)
	}
}

func TestViewFitsTerminalWidth(t *testing.T) {
	long := wt("CatalogNCart-a-very-long-worktree-name-for-reviewing-pull-request-818")
	long.Branch = scan.Branch{Name: "chloe/a-very-long-branch-name-that-goes-on-and-on", Tier: scan.TierPointer, ContainedIn: "origin/chloe/a-very-long-branch-name-that-goes-on-and-on", SameTip: true}
	long.Modified = 3
	m := tui.New(tui.Config{Rows: []*scan.Worktree{long}, DirMode: true, Target: "~/worktrees"})
	send(m, tea.WindowSizeMsg{Width: 80, Height: 24}, key("b"))

	view := m.View()
	for _, line := range strings.Split(view, "\n") {
		if w := ansi.StringWidth(line); w > 80 {
			t.Errorf("line is %d cells wide, want <= 80: %q", w, line)
		}
	}
	if !strings.Contains(view, "CatalogNCart-a-ve") || !strings.Contains(view, "-request-818") {
		t.Errorf("long worktree name should keep both ends:\n%s", view)
	}
	if !strings.Contains(view, "(1 AT RISK)") {
		t.Errorf("header lacks the AT RISK count:\n%s", view)
	}
}
