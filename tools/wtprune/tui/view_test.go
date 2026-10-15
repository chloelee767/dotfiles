package tui_test

import (
	"fmt"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/x/ansi"

	"wtprune/scan"
	"wtprune/tui"
)

var (
	pgDown = tea.KeyMsg{Type: tea.KeyPgDown}
	pgUp   = tea.KeyMsg{Type: tea.KeyPgUp}
	end    = tea.KeyMsg{Type: tea.KeyEnd}
	home   = tea.KeyMsg{Type: tea.KeyHome}
)

func assertFits(t *testing.T, view string, width, height int) {
	t.Helper()
	lines := strings.Split(view, "\n")
	if len(lines) > height {
		t.Errorf("view is %d lines, want <= %d", len(lines), height)
	}
	for _, line := range lines {
		if w := ansi.StringWidth(line); w > width {
			t.Errorf("line is %d cells wide, want <= %d: %q", w, width, line)
		}
	}
}

func lastLine(view string) string {
	lines := strings.Split(view, "\n")
	return lines[len(lines)-1]
}

func TestTypedYesPromptTreatsLettersAsText(t *testing.T) {
	dirty := wt("dirty")
	dirty.Modified = 30
	for i := range 30 {
		dirty.Changes = append(dirty.Changes, scan.Change{Code: "M", Path: fmt.Sprintf("dir/file-with-a-long-name-%02d.go", i)})
	}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{dirty}})
	send(m, tea.WindowSizeMsg{Width: 60, Height: 8}, space, enter)

	send(m, key("j"), key("k"), key("q"))
	view := m.View()
	if !strings.HasPrefix(view, "REMOVE 1 worktree") {
		t.Errorf("j/k scrolled the review:\n%s", view)
	}
	if !strings.Contains(view, "> jkq") {
		t.Errorf("j/k/q were not typed:\n%s", view)
	}

	send(m, down)
	if view := m.View(); strings.HasPrefix(view, "REMOVE 1 worktree") {
		t.Errorf("down arrow did not scroll:\n%s", view)
	}

	if cmd := send(m, backspace, backspace, backspace, key("yes"), enter); !isQuit(cmd) || m.ConfirmedPlan() == nil {
		t.Fatal(`"yes" after deleting the typed letters did not confirm`)
	}
}

func TestReviewScrollKeys(t *testing.T) {
	var rows []*scan.Worktree
	for i := range 20 {
		rows = append(rows, wt(fmt.Sprintf("r%02d", i)))
	}
	m := tui.New(tui.Config{Rows: rows})
	send(m, tea.WindowSizeMsg{Width: 80, Height: 10}, key("a"), enter)

	send(m, pgDown)
	if view := m.View(); strings.Contains(view, "REMOVE") || !strings.Contains(view, "r07") {
		t.Errorf("pgdown should scroll a page:\n%s", view)
	}
	send(m, end)
	if view := m.View(); !strings.Contains(view, "r19") {
		t.Errorf("end should show the last row:\n%s", view)
	}
	send(m, pgUp, home)
	if view := m.View(); !strings.HasPrefix(view, "REMOVE 20 worktrees") {
		t.Errorf("home should scroll to the top:\n%s", view)
	}
	send(m, key("j"))
	if view := m.View(); strings.HasPrefix(view, "REMOVE 20 worktrees") {
		t.Errorf("j should scroll when no typed yes is needed:\n%s", view)
	}
}

func TestListPageKeysMoveTheCursor(t *testing.T) {
	var rows []*scan.Worktree
	for i := range 60 {
		rows = append(rows, wt(fmt.Sprintf("r%02d", i)))
	}
	m := tui.New(tui.Config{Rows: rows})
	send(m, tea.WindowSizeMsg{Width: 100, Height: 30}, pgDown, space)

	sel := m.Selection().Worktrees
	if len(sel) != 1 || sel[0] == "/wt/r00" {
		t.Fatalf("selection after pgdown = %v, want one row past the first page", sel)
	}

	send(m, pgUp, space)
	assertIDs(t, "selection after pgup", m.Selection().Worktrees, []string{"/wt/r00", sel[0]})
}

func TestFilterKeepsTheCursorRow(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a"), wt("b"), wt("foo-c"), wt("d")}})

	send(m, down, down, key("/"), key("foo"), enter, esc, space)
	assertIDs(t, "selection after filtering and clearing", m.Selection().Worktrees, []string{"/wt/foo-c"})
}

func TestOlderThanHiddenCountInHeader(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}, HiddenByAge: 4, Target: "~/" + strings.Repeat("deep/", 30)})
	send(m, tea.WindowSizeMsg{Width: 90, Height: 30})

	view := m.View()
	for _, want := range []string{"1 worktree", "0 selected", "0 branches to delete", "4 hidden by --older-than"} {
		if !strings.Contains(view, want) {
			t.Errorf("header lacks %q:\n%s", want, view)
		}
	}
	assertFits(t, view, 90, 30)
}

func TestFooterKeepsQuitAndHelp(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}})
	for _, width := range []int{40, 60, 90, 200} {
		send(m, tea.WindowSizeMsg{Width: width, Height: 30})
		footer := lastLine(m.View())
		if !strings.Contains(footer, "q quit") || !strings.Contains(footer, "? help") {
			t.Errorf("footer at %d cols lacks q quit / ? help: %q", width, footer)
		}
	}
}

// wideRows mixes wide (CJK, emoji) and long names across two repos.
func wideRows() []*scan.Worktree {
	alpha := &scan.Repo{Name: "alpha", CommonDir: "/src/alpha/.git", MainPath: "/src/alpha"}
	beta := &scan.Repo{Name: "ベータ", CommonDir: "/src/beta/.git", MainPath: "/src/beta"}
	a := &scan.Worktree{
		ID: "/wt/alpha-日本語のブランチ名前-とても長い", Path: "/wt/alpha-日本語のブランチ名前-とても長い", Repo: alpha,
		Branch:     scan.Branch{Name: "chloe/日本語-🌲-feature-with-a-long-name", Tier: scan.TierSquashed, ContainedIn: "origin/master"},
		Modified:   2,
		Changes:    []scan.Change{{Code: "M", Path: "文書/説明.md"}, {Code: "M", Path: "go.mod"}},
		InUse:      []scan.Process{{PID: 1, Command: "zsh"}, {PID: 2, Command: "nvim"}, {PID: 3, Command: "claude"}},
		LastActive: time.Now(), Idle: 30 * 24 * time.Hour,
	}
	b := &scan.Worktree{
		ID: "/wt/ベータ-🌲🌲🌲-review", Path: "/wt/ベータ-🌲🌲🌲-review", Repo: beta,
		Branch: scan.Branch{Name: "review-🌲", Tier: scan.TierPointer, ContainedIn: "origin/feature/🌲-very-long-remote-branch-name", SameTip: true},
	}
	c := &scan.Worktree{
		ID: "/wt/alpha-locked", Path: "/wt/alpha-locked", Repo: alpha, State: scan.StateLocked, Hint: "git worktree unlock /wt/alpha-locked",
		Branch: scan.Branch{Name: "locked", KeepReason: "3 unique"},
	}
	return []*scan.Worktree{a, b, c}
}

func TestViewFitsWithWideUnicode(t *testing.T) {
	for _, size := range [][2]int{{90, 30}, {200, 50}, {160, 45}, {50, 12}} {
		t.Run(fmt.Sprintf("%dx%d", size[0], size[1]), func(t *testing.T) {
			m := tui.New(tui.Config{Rows: wideRows(), DirMode: true, Target: "~/worktrees"})
			send(m, tea.WindowSizeMsg{Width: size[0], Height: size[1]}, key("a"), key("B"))
			assertFits(t, m.View(), size[0], size[1])

			send(m, key("?"))
			assertFits(t, m.View(), size[0], size[1])

			send(m, key("?"), enter)
			if !m.Reviewing() {
				t.Fatal("enter did not open the review")
			}
			assertFits(t, m.View(), size[0], size[1])
		})
	}
}

func TestStatusCellCountsProcessesAndDetailListsThem(t *testing.T) {
	m := tui.New(tui.Config{Rows: wideRows()[:1]})
	send(m, tea.WindowSizeMsg{Width: 160, Height: 45})

	view := m.View()
	if !strings.Contains(view, "IN USE 3") {
		t.Errorf("status cell should say IN USE 3:\n%s", view)
	}
	if !strings.Contains(view, "zsh (pid 1), nvim (pid 2), claude (pid 3)") {
		t.Errorf("detail pane should list the processes:\n%s", view)
	}
}

func TestDirModeColumns(t *testing.T) {
	alpha := &scan.Repo{Name: "alpha", CommonDir: "/src/alpha/.git", MainPath: "/src/alpha"}
	row := &scan.Worktree{ID: "/wt/alpha-review-x", Path: "/wt/alpha-review-x", Repo: alpha, Branch: scan.Branch{Name: "review-x", KeepReason: "1 unique"}}

	m := tui.New(tui.Config{Rows: []*scan.Worktree{row}, DirMode: true})
	send(m, tea.WindowSizeMsg{Width: 160, Height: 30})
	view := m.View()
	if !strings.Contains(view, "REPO") || !strings.Contains(view, "alpha  review-x") {
		t.Errorf("wide dir mode should show REPO and strip the repo prefix:\n%s", view)
	}

	send(m, tea.WindowSizeMsg{Width: 90, Height: 30})
	view = m.View()
	if strings.Contains(view, "REPO") || !strings.Contains(view, "alpha-review-x") {
		t.Errorf("narrow dir mode should drop REPO and show the full name:\n%s", view)
	}
}

func TestBranchActionStaysReadableAtNarrowWidth(t *testing.T) {
	row := wt("feature-with-a-rather-long-worktree-name")
	row.Branch = scan.Branch{Name: "chloe/feature-with-a-rather-long-branch-name", Tier: scan.TierMerged, ContainedIn: "origin/master"}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{row}})
	send(m, tea.WindowSizeMsg{Width: 90, Height: 30})

	if view := m.View(); !strings.Contains(view, "merged (in origin/master)") {
		t.Errorf("branch action should be shown in full at 90 cols:\n%s", view)
	}
}

func TestControlCharactersAreEscaped(t *testing.T) {
	evil := wt("evil")
	evil.Path = "/wt/evil\nname\x1b[2J"
	evil.Branch = scan.Branch{Name: "br\tanch", KeepReason: "1 unique"}
	evil.Untracked = 1
	evil.Changes = []scan.Change{{Code: "??", Path: "new\nfile"}}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{evil}})
	send(m, tea.WindowSizeMsg{Width: 120, Height: 30})

	for _, view := range []string{m.View(), func() string { send(m, space, enter); return m.View() }()} {
		if strings.Contains(view, "\x1b[2J") || strings.Contains(view, "\t") {
			t.Errorf("view contains raw control characters:\n%q", view)
		}
		if !strings.Contains(view, `evil\nname\x1b[2J`) {
			t.Errorf("view lacks the escaped name:\n%s", view)
		}
		assertFits(t, view, 120, 30)
	}
}

func TestDetailShowsBranchStateLostAndReflogOnly(t *testing.T) {
	gone := wt("gone")
	gone.Branch = scan.Branch{Name: "gone", Upstream: "origin/gone", UpstreamGone: true, KeepReason: "1 unique"}
	gone.ReflogOnly = []scan.Commit{{SHA: "1111111aaaa", Subject: "rebased away"}}
	gone.Ignored = []scan.IgnoredEntry{{Path: ".env"}, {Path: "node_modules/", Dir: true, Regenerable: true}}
	gone.IgnoredFiles = []string{".env"}
	detached := wt("detached")
	detached.Detached, detached.Head = true, "2222222bbbb"
	detached.Branch = scan.Branch{KeepReason: "detached"}
	detached.DetachedLost = 1
	lost := map[string][]scan.Commit{detached.ID: {{SHA: "2222222bbbb", Subject: "only here"}}}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{gone, detached}, Lost: lost})
	send(m, tea.WindowSizeMsg{Width: 120, Height: 40})

	view := m.View()
	for _, want := range []string{"origin/gone [gone]", "1 commit only in reflogs", "1111111 rebased away", ".env", "node_modules/"} {
		if !strings.Contains(view, want) {
			t.Errorf("detail for gone lacks %q:\n%s", want, view)
		}
	}
	send(m, down)
	if view := m.View(); !strings.Contains(view, "2222222 only here") {
		t.Errorf("detail for detached lacks the lost commit:\n%s", view)
	}
}

func TestReviewSections(t *testing.T) {
	alpha := &scan.Repo{Name: "alpha", CommonDir: "/src/alpha/.git", MainPath: "/src/alpha"}
	merged := &scan.Worktree{ID: "/wt/merged", Path: "/wt/merged", Repo: alpha,
		Branch: scan.Branch{Name: "m", Tier: scan.TierMerged, ContainedIn: "origin/master"}}
	beta := &scan.Repo{Name: "beta", CommonDir: "/src/beta/.git", MainPath: "/src/beta"}
	pointer := &scan.Worktree{ID: "/wt/pointer", Path: "/wt/pointer", Repo: beta,
		Branch: scan.Branch{Name: "a-much-longer-branch-name", Tier: scan.TierPointer, ContainedIn: "origin/x", SameTip: true}}
	kept := &scan.Worktree{ID: "/wt/kept", Path: "/wt/kept", Repo: alpha, Branch: scan.Branch{Name: "kept", KeepReason: "2 unique"}}
	detached := &scan.Worktree{ID: "/wt/detached", Path: "/wt/detached", Repo: alpha, Detached: true, Head: "3333333cccc",
		Branch: scan.Branch{KeepReason: "detached"}, DetachedLost: 1}
	lost := map[string][]scan.Commit{detached.ID: {{SHA: "3333333cccc", Subject: "lost work"}}}
	m := tui.New(tui.Config{Rows: []*scan.Worktree{merged, pointer, kept, detached}, Lost: lost, DirMode: true})
	send(m, tea.WindowSizeMsg{Width: 160, Height: 45}, key("B"), key("a"), enter)

	view := m.View()
	for _, want := range []string{"LOST 3333333 lost work", "KEEP 1 branch", "kept", "2 unique", "DELETE 2 branches"} {
		if !strings.Contains(view, want) {
			t.Errorf("review lacks %q:\n%s", want, view)
		}
	}
	var labelCols []int
	for _, line := range strings.Split(view, "\n") {
		for _, label := range []string{"merged (in origin/master)", "pointer (= origin/x)"} {
			if i := strings.Index(line, label); i >= 0 {
				labelCols = append(labelCols, ansi.StringWidth(line[:i]))
			}
		}
	}
	if len(labelCols) != 2 || labelCols[0] != labelCols[1] {
		t.Errorf("DELETE labels should start in one column, got columns %v:\n%s", labelCols, view)
	}
}

func TestStatusLineShowsWarningsAndSkipped(t *testing.T) {
	m := tui.New(tui.Config{
		Rows: []*scan.Worktree{wt("a")}, DirMode: true,
		Warnings: []string{"lsof failed: not found"},
		Skipped:  scan.Skipped{Clones: 1, ClonePaths: []string{"/wt/a-clone"}},
	})
	send(m, tea.WindowSizeMsg{Width: 160, Height: 40})

	view := m.View()
	if !strings.Contains(view, "lsof failed: not found") || !strings.Contains(view, "1 clone") {
		t.Errorf("status line lacks the warning or skipped count:\n%s", view)
	}
	send(m, key("?"), key("G"))
	if view := m.View(); !strings.Contains(view, "/wt/a-clone") {
		t.Errorf("help lacks the skipped clone path:\n%s", view)
	}
}

func TestHelpHasLegend(t *testing.T) {
	m := tui.New(tui.Config{Rows: []*scan.Worktree{wt("a")}})
	send(m, tea.WindowSizeMsg{Width: 120, Height: 60}, key("?"))

	view := m.View()
	for _, want := range []string{"IDLE", "squash-merged", "contained", "pointer", "pushed", "HIDDEN", "LOST", "+N ignored"} {
		if !strings.Contains(view, want) {
			t.Errorf("help lacks %q:\n%s", want, view)
		}
	}
	send(m, key("?"))
	if view := m.View(); strings.Contains(view, "squash-merged") {
		t.Errorf("? did not close the help:\n%s", view)
	}
}
