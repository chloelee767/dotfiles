// Package tui is the interactive selection and review screen. The model
// has no I/O: Update is a pure state transition over key and resize
// messages, so tests drive it without a terminal.
package tui

import (
	"fmt"
	"path/filepath"
	"strings"

	tea "github.com/charmbracelet/bubbletea"

	"wtprune/internal/text"
	"wtprune/plan"
	"wtprune/scan"
)

// Config is what the model starts from.
type Config struct {
	Target         string // shown in the header
	DirMode        bool   // shows the REPO column
	Rows           []*scan.Worktree
	HiddenByAge    int  // rows dropped by --older-than, for the header
	SelectClean    bool // --select clean: start with SAFE rows selected
	DeleteBranches bool // --delete-branches: start with B (auto branch deletion) on
	NoTrash        bool
	TrashDir       string // shown on the review screen

	Lost     map[string][]scan.Commit // worktree ID -> commits removal would lose (see scan.LostCommits)
	Warnings []string                 // scan warnings, shown in the status line and help
	Skipped  scan.Skipped             // --dir: what discovery did not list, shown in the help
}

type screen int

const (
	screenList screen = iota
	screenReview
)

// Model is the bubbletea model. Use it through a pointer.
type Model struct {
	cfg  Config
	rows []*scan.Worktree

	selected map[string]bool // worktree ID -> selected for removal
	// deleteBranch flags a selected row's branch for deletion too.
	// Deselecting a row clears its flag.
	deleteBranch map[string]bool
	// autoBranches (B, --delete-branches) flags the bulk-deletable branch
	// of every row as it gets selected.
	autoBranches bool

	visible    []int // indices into rows matching the filter
	cursor     int   // index into visible
	offset     int   // first visible row shown
	filter     string
	filtering  bool // typing into the filter
	help       bool
	helpOffset int
	hint       string // one-shot message under the list

	screen       screen
	plan         *plan.Plan
	reviewOffset int
	typed        string // typed confirmation text
	confirmed    bool

	width, height int
}

// New builds the model with the initial selection from cfg.
func New(cfg Config) *Model {
	m := &Model{
		cfg:          cfg,
		rows:         cfg.Rows,
		selected:     map[string]bool{},
		deleteBranch: map[string]bool{},
		autoBranches: cfg.DeleteBranches,
		width:        100,
		height:       30,
	}
	for _, w := range cfg.Rows {
		if cfg.SelectClean && w.Safe() {
			m.selectRow(w)
		}
	}
	m.applyFilter()
	return m
}

// Init implements tea.Model.
func (m *Model) Init() tea.Cmd { return nil }

// Selection is what is currently picked: selected worktrees in row order,
// plus the worktrees among them whose branch is flagged.
func (m *Model) Selection() plan.Selection {
	var sel plan.Selection
	for _, w := range m.rows {
		if !m.selected[w.ID] {
			continue
		}
		sel.Worktrees = append(sel.Worktrees, w.ID)
		if m.deleteBranch[w.ID] {
			sel.Branches = append(sel.Branches, w.ID)
		}
	}
	return sel
}

// ConfirmedPlan is the plan the user reviewed and confirmed, or nil if they
// quit without confirming.
func (m *Model) ConfirmedPlan() *plan.Plan {
	if !m.confirmed {
		return nil
	}
	return m.plan
}

// Reviewing reports whether the review/confirm screen is showing.
func (m *Model) Reviewing() bool { return m.screen == screenReview }

// Hint is the one-shot message currently shown, if any.
func (m *Model) Hint() string { return m.hint }

// Update implements tea.Model.
func (m *Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		m.scrollList()
		m.scrollReview(0)
		m.scrollHelp(0)
	case tea.KeyMsg:
		if msg.Type == tea.KeyCtrlC {
			return m, tea.Quit
		}
		switch {
		case m.screen == screenReview:
			return m, m.reviewKey(msg)
		case m.help:
			return m, m.helpKey(msg)
		case m.filtering:
			m.filterKey(msg)
		default:
			return m, m.listKey(msg)
		}
	}
	return m, nil
}

func (m *Model) listKey(msg tea.KeyMsg) tea.Cmd {
	m.hint = ""
	switch msg.String() {
	case "q":
		return tea.Quit
	case "up", "k":
		m.cursor--
	case "down", "j":
		m.cursor++
	case "pgup":
		m.cursor -= m.listHeight()
	case "pgdown":
		m.cursor += m.listHeight()
	case "g", "home":
		m.cursor = 0
	case "G", "end":
		m.cursor = len(m.visible) - 1
	case " ":
		m.toggleRow()
	case "a":
		for _, w := range m.visibleRows() {
			if w.Selectable() {
				m.selectRow(w)
			}
		}
	case "n":
		clear(m.selected)
		clear(m.deleteBranch)
	case "c":
		for _, w := range m.visibleRows() {
			switch {
			case w.Safe():
				m.selectRow(w)
			case w.Selectable():
				m.deselectRow(w)
			}
		}
	case "b":
		m.toggleBranch()
	case "B":
		m.toggleAutoBranches()
	case "/":
		m.filtering = true
	case "esc":
		m.setFilter("")
	case "enter":
		m.openReview()
	case "?":
		m.help = true
		m.helpOffset = 0
	}
	m.scrollList()
	return nil
}

func (m *Model) helpKey(msg tea.KeyMsg) tea.Cmd {
	switch msg.String() {
	case "q":
		return tea.Quit
	case "?", "esc", "enter":
		m.help = false
	case "up", "k":
		m.scrollHelp(-1)
	case "down", "j":
		m.scrollHelp(1)
	case "pgup":
		m.scrollHelp(-m.bodyHeight())
	case "pgdown", " ":
		m.scrollHelp(m.bodyHeight())
	case "g", "home":
		m.scrollHelp(-m.helpOffset)
	case "G", "end":
		m.scrollHelp(len(m.helpLines()))
	}
	return nil
}

func (m *Model) filterKey(msg tea.KeyMsg) {
	switch msg.Type {
	case tea.KeyEsc:
		m.filtering = false
		m.setFilter("")
	case tea.KeyEnter:
		m.filtering = false
	case tea.KeyBackspace:
		r := []rune(m.filter)
		if len(r) > 0 {
			m.setFilter(string(r[:len(r)-1]))
		}
	case tea.KeyRunes:
		m.setFilter(m.filter + string(msg.Runes))
	case tea.KeySpace:
		m.setFilter(m.filter + " ")
	case tea.KeyUp:
		m.cursor--
	case tea.KeyDown:
		m.cursor++
	case tea.KeyPgUp:
		m.cursor -= m.listHeight()
	case tea.KeyPgDown:
		m.cursor += m.listHeight()
	}
	m.scrollList()
}

func (m *Model) reviewKey(msg tea.KeyMsg) tea.Cmd {
	switch msg.Type {
	case tea.KeyEsc:
		m.closeReview()
		return nil
	case tea.KeyUp:
		m.scrollReview(-1)
		return nil
	case tea.KeyDown:
		m.scrollReview(1)
		return nil
	case tea.KeyPgUp:
		m.scrollReview(-m.reviewHeight())
		return nil
	case tea.KeyPgDown:
		m.scrollReview(m.reviewHeight())
		return nil
	case tea.KeyHome:
		m.scrollReview(-m.reviewOffset)
		return nil
	case tea.KeyEnd:
		m.scrollReview(len(m.reviewLines()))
		return nil
	}

	if m.plan.NeedsTypedYes() {
		switch msg.Type {
		case tea.KeyRunes:
			m.typed += string(msg.Runes)
			m.hint = ""
		case tea.KeySpace:
			m.typed += " "
		case tea.KeyBackspace:
			if r := []rune(m.typed); len(r) > 0 {
				m.typed = string(r[:len(r)-1])
			}
		case tea.KeyEnter:
			if strings.EqualFold(strings.TrimSpace(m.typed), "yes") {
				m.confirmed = true
				return tea.Quit
			}
			m.hint = fmt.Sprintf(`not confirmed: type "yes" in full, got %q`, m.typed)
			m.typed = ""
		}
		return nil
	}

	m.hint = ""
	switch msg.String() {
	case "y":
		m.confirmed = true
		return tea.Quit
	case "n":
		m.closeReview()
	case "q":
		return tea.Quit
	case "j":
		m.scrollReview(1)
	case "k":
		m.scrollReview(-1)
	case "g":
		m.scrollReview(-m.reviewOffset)
	case "G":
		m.scrollReview(len(m.reviewLines()))
	case "enter":
		m.hint = "press y to confirm, esc to go back"
	}
	return nil
}

func (m *Model) selectRow(w *scan.Worktree) {
	m.selected[w.ID] = true
	if m.autoBranches && w.Branch.Bulk() {
		m.deleteBranch[w.ID] = true
	}
}

func (m *Model) deselectRow(w *scan.Worktree) {
	delete(m.selected, w.ID)
	delete(m.deleteBranch, w.ID)
}

func (m *Model) toggleRow() {
	w := m.current()
	switch {
	case w == nil:
	case !w.Selectable():
		m.hint = blockedHint(w)
	case m.selected[w.ID]:
		m.deselectRow(w)
	default:
		m.selectRow(w)
	}
}

// toggleBranch flips "delete branch" on the cursor row, for any candidate
// incl. pushed; turning it on also selects the row, since the branch only
// goes if its worktree does.
func (m *Model) toggleBranch() {
	w := m.current()
	switch {
	case w == nil:
	case !w.Selectable():
		m.hint = blockedHint(w)
	case !w.Branch.Candidate():
		m.hint = "branch is kept: " + keepReason(w)
	case m.deleteBranch[w.ID]:
		delete(m.deleteBranch, w.ID)
	default:
		m.deleteBranch[w.ID] = true
		m.selected[w.ID] = true
	}
}

// toggleAutoBranches flips B. On: flags the bulk-deletable branch of every
// selected row shown, and of every row selected from now on. Off: clears
// every flag.
func (m *Model) toggleAutoBranches() {
	m.autoBranches = !m.autoBranches
	if !m.autoBranches {
		clear(m.deleteBranch)
		m.hint = "B off: no branches will be deleted (b flags one row's branch)"
		return
	}
	for _, w := range m.visibleRows() {
		if m.selected[w.ID] && w.Branch.Bulk() {
			m.deleteBranch[w.ID] = true
		}
	}
	m.hint = "B on: selecting a row also deletes its merged/contained/pointer/squash-merged branch"
}

func (m *Model) openReview() {
	p := plan.Build(m.rows, m.Selection(), m.cfg.NoTrash)
	if p.Empty() {
		m.hint = "nothing selected: space selects the row, a selects all shown, c selects the safe ones"
		return
	}
	m.plan = p
	m.screen = screenReview
	m.reviewOffset = 0
	m.typed = ""
	m.hint = ""
}

func (m *Model) closeReview() {
	m.screen = screenList
	m.plan = nil
	m.typed = ""
	m.hint = ""
}

func (m *Model) setFilter(f string) {
	var keep string
	if w := m.current(); w != nil {
		keep = w.ID
	}
	m.filter = f
	m.applyFilter()
	m.cursor = 0
	for i, idx := range m.visible {
		if m.rows[idx].ID == keep {
			m.cursor = i
		}
	}
}

func (m *Model) applyFilter() {
	m.visible = m.visible[:0]
	q := strings.ToLower(m.filter)
	for i, w := range m.rows {
		if q == "" || strings.Contains(strings.ToLower(filterText(w)), q) {
			m.visible = append(m.visible, i)
		}
	}
}

func filterText(w *scan.Worktree) string {
	return text.Printable(w.Repo.Name + "\x00" + filepath.Base(w.Path) + "\x00" + w.Branch.Name)
}

func (m *Model) visibleRows() []*scan.Worktree {
	out := make([]*scan.Worktree, len(m.visible))
	for i, idx := range m.visible {
		out[i] = m.rows[idx]
	}
	return out
}

func (m *Model) current() *scan.Worktree {
	if m.cursor < 0 || m.cursor >= len(m.visible) {
		return nil
	}
	return m.rows[m.visible[m.cursor]]
}

// scrollList clamps the cursor and scrolls so the cursor row stays visible.
func (m *Model) scrollList() {
	m.cursor = max(0, min(m.cursor, len(m.visible)-1))
	h := m.listHeight()
	if m.cursor < m.offset {
		m.offset = m.cursor
	}
	if m.cursor >= m.offset+h {
		m.offset = m.cursor - h + 1
	}
	m.offset = max(0, min(m.offset, len(m.visible)-h))
}

func (m *Model) scrollHelp(delta int) {
	if !m.help {
		return
	}
	maxOffset := max(0, len(m.helpLines())-m.bodyHeight())
	m.helpOffset = max(0, min(m.helpOffset+delta, maxOffset))
}

func (m *Model) scrollReview(delta int) {
	if m.screen != screenReview {
		return
	}
	maxOffset := max(0, len(m.reviewLines())-m.reviewHeight())
	m.reviewOffset = max(0, min(m.reviewOffset+delta, maxOffset))
}

func blockedHint(w *scan.Worktree) string {
	if w.Hint == "" {
		return fmt.Sprintf("blocked (%s): cannot be removed", w.State)
	}
	return fmt.Sprintf("blocked (%s): %s", w.State, text.Printable(w.Hint))
}

func keepReason(w *scan.Worktree) string {
	if r := w.Branch.Label(); r != "" {
		return r
	}
	return "not a deletion candidate"
}
