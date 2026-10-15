package tui

import (
	"cmp"
	"fmt"
	"path/filepath"
	"slices"
	"strings"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/x/ansi"

	"wtprune/internal/text"
	"wtprune/plan"
	"wtprune/scan"
)

// Screen rows outside the list: two header lines, column titles, two
// separators, status line, keys line.
const listChrome = 7

const (
	maxDetailLines = 10
	minListRows    = 10
	maxChanges     = 8
	maxDetailLost  = 3
	maxReviewLost  = 20
	maxReflogShown = 5
	rowPrefixWidth = 6 // "> [x] "
	columnGap      = "  "
	maxStatusWidth = 26
	// wideWidth is the width from which dir mode shows a REPO column (and
	// strips the repo prefix from worktree names).
	wideWidth = 110
)

func (m *Model) detailHeight() int {
	return min(maxDetailLines, max(0, m.height-listChrome-minListRows))
}

func (m *Model) listHeight() int { return max(1, m.height-listChrome-m.detailHeight()) }

// bodyHeight is everything between the header and the footer (help screen).
func (m *Model) bodyHeight() int { return max(1, m.height-5) }

// reviewHeight is the scrollable part of the review screen: all but a
// separator, the prompt and the input/hint line.
func (m *Model) reviewHeight() int { return max(1, m.height-3) }

func (m *Model) showRepoColumn() bool { return m.cfg.DirMode && m.width >= wideWidth }

// View implements tea.Model.
func (m *Model) View() string {
	if m.screen == screenReview {
		return m.reviewView()
	}
	return m.listView()
}

func (m *Model) listView() string {
	h1, h2 := m.header()
	lines := []string{h1, h2}
	sep := styleDim.Render(strings.Repeat("─", m.width))

	if m.help {
		help := m.helpLines()
		for i := range m.bodyHeight() {
			line := ""
			if j := m.helpOffset + i; j < len(help) {
				line = truncEnd(help[j], m.width)
			}
			lines = append(lines, line)
		}
	} else {
		cols := m.columns()
		lines = append(lines, fitLine(m.titleRow(cols), m.width))
		h := m.listHeight()
		for i := m.offset; i < min(m.offset+h, len(m.visible)); i++ {
			lines = append(lines, fitLine(m.row(m.rows[m.visible[i]], cols, i == m.cursor), m.width))
		}
		if len(m.visible) == 0 {
			lines = append(lines, styleDim.Render(" no worktrees match the filter"))
		}
		for len(lines) < 3+h {
			lines = append(lines, "")
		}
		lines = append(lines, sep)
		detail := m.detailLines()
		for i := range m.detailHeight() {
			line := ""
			if i < len(detail) {
				line = truncEnd(detail[i], m.width)
			}
			lines = append(lines, line)
		}
	}
	lines = append(lines, sep, truncEnd(m.statusLine(), m.width), m.keysLine())
	return strings.Join(lines, "\n")
}

// header is one line when it fits, else the path on the first line and the
// counts on the second: the counts are never cut for the path's sake.
func (m *Model) header() (string, string) {
	selected, atRisk, branches := 0, 0, 0
	repos := map[string]bool{}
	for _, w := range m.rows {
		repos[w.Repo.CommonDir] = true
		if !m.selected[w.ID] {
			continue
		}
		selected++
		if w.AtRisk() {
			atRisk++
		}
		if m.deleteBranch[w.ID] && w.Branch.Candidate() {
			branches++
		}
	}
	count := fmt.Sprintf("%d %s", len(m.rows), text.Plural(len(m.rows), "worktree", "worktrees"))
	if m.cfg.DirMode {
		count += fmt.Sprintf(" in %d %s", len(repos), text.Plural(len(repos), "repo", "repos"))
	}
	sel := fmt.Sprintf("%d selected", selected)
	if atRisk > 0 {
		sel += " " + styleAtRisk.Render(fmt.Sprintf("(%d AT RISK)", atRisk))
	}
	branchCount := fmt.Sprintf("%d %s to delete", branches, text.Plural(branches, "branch", "branches"))
	if m.autoBranches {
		branchCount += " " + styleDelete.Render("(B: auto)")
	}
	counts := strings.Join([]string{count, sel, branchCount}, "  ·  ")
	title := styleBold.Render("wtprune") + "  "
	target := text.Printable(m.cfg.Target)
	if m.cfg.HiddenByAge > 0 {
		counts += "  ·  " + styleDim.Render(fmt.Sprintf("%d hidden by --older-than", m.cfg.HiddenByAge))
	}

	const minTarget = 20
	oneLine := m.width - ansi.StringWidth(title) - len("  ·  ") - ansi.StringWidth(counts)
	if oneLine >= min(minTarget, ansi.StringWidth(target)) {
		return title + elideMiddle(target, oneLine) + "  ·  " + counts, ""
	}
	// Two lines: the hidden count moves up beside the (elided) path, so the
	// selection counts below keep their full width.
	first, second := title, strings.Join([]string{count, sel, branchCount}, " · ")
	if m.cfg.HiddenByAge > 0 {
		hidden := styleDim.Render(fmt.Sprintf("%d hidden by --older-than", m.cfg.HiddenByAge))
		first += elideMiddle(target, max(minTarget, m.width-ansi.StringWidth(title)-3-ansi.StringWidth(hidden))) + " · " + hidden
	} else {
		first += elideMiddle(target, m.width-ansi.StringWidth(title))
	}
	return truncEnd(first, m.width), truncEnd(" "+second, m.width)
}

type colKind int

const (
	colRepo colKind = iota
	colName
	colBranch
	colIdle
	colStatus
	colAction
)

type column struct {
	kind  colKind
	title string
	width int
}

// columns sizes the table to the terminal: natural widths over all rows (so
// filtering does not shift columns), then the column widest relative to its
// weight is shaved one cell at a time until the row fits or every column is
// at its minimum. BRANCH ACTION has a high minimum and BRANCH a low weight:
// what happens to the branch matters more than its full name, which the
// detail pane shows.
func (m *Model) columns() []column {
	cols := []column{
		{kind: colName, title: "WORKTREE"},
		{kind: colBranch, title: "BRANCH"},
		{kind: colIdle, title: "IDLE"},
		{kind: colStatus, title: "STATUS"},
		{kind: colAction, title: "BRANCH ACTION"},
	}
	if m.showRepoColumn() {
		cols = append([]column{{kind: colRepo, title: "REPO"}}, cols...)
	}
	minWidth := map[colKind]int{colRepo: 6, colName: 14, colBranch: 10, colIdle: 4, colStatus: 12, colAction: 28}
	weightPct := map[colKind]int{colRepo: 80, colName: 200, colBranch: 100, colIdle: 100, colStatus: 100, colAction: 300}
	mins := make([]int, len(cols))
	for i := range cols {
		cols[i].width = ansi.StringWidth(cols[i].title)
		for _, w := range m.rows {
			cols[i].width = max(cols[i].width, ansi.StringWidth(m.cellText(w, cols[i].kind)))
		}
		if cols[i].kind == colStatus {
			cols[i].width = min(cols[i].width, maxStatusWidth)
		}
		mins[i] = min(minWidth[cols[i].kind], cols[i].width)
	}

	avail := m.width - rowPrefixWidth - len(columnGap)*(len(cols)-1)
	for {
		total, widest := 0, -1
		relative := func(i int) int { return cols[i].width * 100 / weightPct[cols[i].kind] }
		for i, c := range cols {
			total += c.width
			if c.width > mins[i] && (widest < 0 || relative(i) > relative(widest)) {
				widest = i
			}
		}
		if total <= avail || widest < 0 {
			return cols
		}
		cols[widest].width--
	}
}

func (m *Model) titleRow(cols []column) string {
	cells := make([]string, len(cols))
	for i, c := range cols {
		if c.kind == colIdle {
			cells[i] = styleTitle.Render(padLeft(c.title, c.width))
		} else {
			cells[i] = styleTitle.Render(truncEnd(c.title, c.width)) + strings.Repeat(" ", max(0, c.width-len(c.title)))
		}
	}
	return strings.Repeat(" ", rowPrefixWidth) + strings.Join(cells, columnGap)
}

func (m *Model) row(w *scan.Worktree, cols []column, isCursor bool) string {
	// Reverse video rather than a background colour keeps red at-risk text
	// readable on the cursor row.
	paint := func(st lipgloss.Style, s string) string {
		if isCursor {
			st = st.Reverse(true)
		}
		return st.Render(s)
	}
	plain := lipgloss.NewStyle()
	dimRow := !w.Selectable()

	cursor, box := " ", "[ ]"
	if isCursor {
		cursor = ">"
	}
	boxStyle := plain
	switch {
	case dimRow:
		box, boxStyle = "[-]", styleDim
	case m.selected[w.ID] && w.AtRisk():
		box, boxStyle = "[x]", styleAtRisk
	case m.selected[w.ID]:
		box, boxStyle = "[x]", styleBold
	}

	var b strings.Builder
	b.WriteString(paint(styleBold, cursor) + paint(plain, " ") + paint(boxStyle, box) + paint(plain, " "))
	for i, c := range cols {
		if i > 0 {
			b.WriteString(paint(plain, columnGap))
		}
		cell := m.cellText(w, c.kind)
		switch c.kind {
		case colName, colBranch, colRepo:
			cell = pad(elideMiddle(cell, c.width), c.width)
		case colIdle:
			cell = padLeft(cell, c.width)
		default:
			cell = pad(truncEnd(cell, c.width), c.width)
		}
		st := styleDim
		if !dimRow {
			st = m.cellStyle(w, c.kind)
		}
		b.WriteString(paint(st, cell))
	}
	return b.String()
}

func (m *Model) cellStyle(w *scan.Worktree, kind colKind) lipgloss.Style {
	switch kind {
	case colName:
		if w.AtRisk() {
			return styleRiskText
		}
	case colStatus:
		switch {
		case w.AtRisk():
			return styleAtRisk
		case w.IgnoredCount() > 0:
			return styleNote
		default:
			return styleSafe
		}
	case colAction:
		switch {
		case !w.Branch.Candidate():
			return lipgloss.NewStyle()
		case m.deleteBranch[w.ID]:
			return styleDelete
		default:
			return styleDim
		}
	}
	return lipgloss.NewStyle()
}

// cellText is the unstyled, untruncated text of one cell.
func (m *Model) cellText(w *scan.Worktree, kind colKind) string {
	switch kind {
	case colRepo:
		return text.Printable(w.Repo.Name)
	case colName:
		name := filepath.Base(w.Path)
		if rest, ok := strings.CutPrefix(name, w.Repo.Name+"-"); ok && rest != "" && m.showRepoColumn() {
			name = rest
		}
		return text.Printable(name)
	case colBranch:
		switch {
		case w.Detached:
			return "(detached)"
		case w.Branch.Name == "":
			return "--"
		}
		return text.Printable(w.Branch.Name)
	case colIdle:
		return w.IdleText()
	case colStatus:
		return statusCell(w)
	case colAction:
		return actionText(w, m.deleteBranch[w.ID], m.width < wideWidth)
	}
	return ""
}

// statusCell is StatusText with IN USE as a process count: the command
// list is in the detail pane.
func statusCell(w *scan.Worktree) string {
	if !w.Selectable() {
		return string(w.State)
	}
	flags := w.RiskFlags()
	for i, f := range flags {
		if strings.HasPrefix(f, "IN USE ") {
			flags[i] = fmt.Sprintf("IN USE %d", len(w.InUse))
		}
	}
	status := "clean"
	if len(flags) > 0 {
		status = strings.Join(flags, " ")
	}
	if n := w.IgnoredCount(); n > 0 {
		status += fmt.Sprintf(" +%d ignored", n)
	}
	return status
}

// ActionText is the BRANCH ACTION cell: "delete <label>" when flagged,
// "-- <label>" for a candidate not flagged, "keep <reason>" otherwise.
// Blocked and detached rows show nothing.
func ActionText(w *scan.Worktree, flagged bool) string { return actionText(w, flagged, false) }

func actionText(w *scan.Worktree, flagged, short bool) string {
	verb, verbW := "delete", 8
	if short {
		verb, verbW = "del", 5
	}
	label := text.Printable(w.Branch.Label())
	switch {
	case !w.Selectable() || label == "":
		return ""
	case !w.Branch.Candidate():
		return pad("keep", verbW) + label
	case flagged:
		return pad(verb, verbW) + label
	}
	return pad("--", verbW) + label
}

func (m *Model) detailLines() []string {
	w := m.current()
	if w == nil {
		return nil
	}
	field := func(label, value string) string {
		return " " + styleDim.Render(pad(label, 9)) + value
	}
	lines := []string{" " + styleBold.Render(elideMiddle(text.Printable(text.Tilde(w.Path)), m.width-2))}
	if !w.Selectable() {
		lines = append(lines, field("blocked", styleBold.Render(string(w.State))+"  "+text.Printable(w.Hint)))
	}
	if flags := w.RiskFlags(); len(flags) > 0 {
		lines = append(lines, field("AT RISK", styleAtRisk.Render(text.Printable(strings.Join(flags, "  ")))))
	}
	repo := cmp.Or(w.Repo.MainPath, w.Repo.CommonDir)
	lines = append(lines, field("repo", text.Printable(text.Tilde(repo))))
	lines = append(lines, field("branch", branchDetail(w)))
	if c := w.LastCommit; c.SHA != "" {
		lines = append(lines, field("last", fmt.Sprintf("%s  %s  %s", text.Short(c.SHA), c.Time.Format("2006-01-02"), text.Printable(c.Subject))))
	}
	if n := len(w.Changes); n > 0 {
		var shown []string
		for _, c := range w.Changes[:min(n, maxChanges)] {
			shown = append(shown, text.Printable(c.String()))
		}
		value := strings.Join(shown, "   ")
		if n > maxChanges {
			value = fmt.Sprintf("(%d) %s", n, value)
		}
		lines = append(lines, field("changes", value))
	}
	lost := m.cfg.Lost[w.ID]
	for i, c := range lost[:min(len(lost), maxDetailLost)] {
		label := ""
		if i == 0 {
			label = "LOST"
		}
		lines = append(lines, field(label, styleRiskText.Render(commitLine(c))))
	}
	if len(lost) > maxDetailLost {
		lines = append(lines, field("", styleDim.Render(fmt.Sprintf("+%d more (the review lists them)", len(lost)-maxDetailLost))))
	}
	if len(w.Hidden) > 0 {
		lines = append(lines, field("hidden", text.Printable(strings.Join(w.Hidden, "  "))))
	}
	if len(w.InUse) > 0 {
		lines = append(lines, field("in use", text.Printable(processList(w.InUse))))
	}
	if len(w.Ignored) > 0 {
		var entries []string
		for _, e := range w.Ignored {
			st := styleNote
			if e.Regenerable {
				st = styleDim
			}
			entries = append(entries, st.Render(text.Printable(e.Path)))
		}
		lines = append(lines, field("ignored", strings.Join(entries, "  ")))
	}
	if len(w.ReflogOnly) > 0 {
		lines = append(lines, field("reflog", styleNote.Render(reflogNote(w.ReflogOnly))))
	}
	if len(w.Nested) > 0 {
		lines = append(lines, field("nested", text.Printable(strings.Join(w.Nested, "  "))))
	}
	if w.Locked && w.LockReason != "" {
		lines = append(lines, field("locked", text.Printable(w.LockReason)))
	}
	return lines
}

func commitLine(c scan.Commit) string {
	return text.Short(c.SHA) + " " + text.Printable(c.Subject)
}

func processList(procs []scan.Process) string {
	var parts []string
	for _, p := range procs {
		parts = append(parts, fmt.Sprintf("%s (pid %d)", p.Command, p.PID))
	}
	return strings.Join(parts, ", ")
}

// reflogNote explains commits only the reflogs hold: not at risk (rebases
// leave them all the time), but recorded in the log so they can be rescued.
func reflogNote(commits []scan.Commit) string {
	var shown []string
	for _, c := range commits[:min(len(commits), maxReflogShown)] {
		shown = append(shown, commitLine(c))
	}
	more := ""
	if n := len(commits) - maxReflogShown; n > 0 {
		more = fmt.Sprintf(", +%d more", n)
	}
	return fmt.Sprintf("%d %s only in reflogs (recorded in the log): %s%s",
		len(commits), text.Plural(len(commits), "commit", "commits"), strings.Join(shown, ", "), more)
}

func branchDetail(w *scan.Worktree) string {
	b := w.Branch
	if w.Detached {
		return "(detached at " + text.Short(w.Head) + ")"
	}
	s := text.Printable(b.Name)
	if b.Upstream != "" {
		s += " -> " + text.Printable(b.Upstream)
		switch {
		case b.UpstreamGone:
			s += " [gone]"
		case b.Ahead > 0 || b.Behind > 0:
			var ab []string
			if b.Ahead > 0 {
				ab = append(ab, fmt.Sprintf("ahead %d", b.Ahead))
			}
			if b.Behind > 0 {
				ab = append(ab, fmt.Sprintf("behind %d", b.Behind))
			}
			s += " (" + strings.Join(ab, ", ") + ")"
		}
	}
	if label := text.Printable(b.Label()); label != "" {
		verb := "keep"
		if b.Candidate() {
			verb = "deletable"
		}
		if b.Tier == scan.TierSquashed && b.SquashCommit != "" {
			label += " as " + text.Short(b.SquashCommit)
		}
		s += styleDim.Render("   " + verb + ": " + label)
	}
	return s
}

func (m *Model) statusLine() string {
	switch {
	case m.help:
		return styleDim.Render(" help: up/down pgup/pgdn scroll  ·  ? or esc closes")
	case m.filtering:
		return " /" + text.Printable(m.filter) + "█" + styleDim.Render(fmt.Sprintf("   %d of %d  ·  enter keep  ·  esc clear", len(m.visible), len(m.rows)))
	case m.hint != "":
		return " " + styleHint.Render(m.hint)
	case m.filter != "":
		return styleDim.Render(fmt.Sprintf(" filter %q: %d of %d shown  ·  esc clears", m.filter, len(m.visible), len(m.rows)))
	}
	var parts []string
	if n := len(m.cfg.Warnings); n > 0 {
		warning := "warning: " + text.Printable(m.cfg.Warnings[0])
		if n > 1 {
			warning += fmt.Sprintf(" (+%d more, see ?)", n-1)
		}
		parts = append(parts, styleHint.Render(warning))
	}
	if skipped := skippedText(m.cfg.Skipped); m.cfg.DirMode && skipped != "" {
		parts = append(parts, styleDim.Render("not listed: "+skipped+" (see ?)"))
	}
	if len(parts) == 0 {
		return ""
	}
	return " " + strings.Join(parts, styleDim.Render("  ·  "))
}

func skippedText(s scan.Skipped) string {
	var parts []string
	for _, p := range []struct {
		n         int
		one, many string
	}{
		{s.Clones, "clone/main worktree", "clones/main worktrees"},
		{s.OtherGit, "other .git file", "other .git files"},
		{s.NonGit, "non-git dir", "non-git dirs"},
	} {
		if p.n > 0 {
			parts = append(parts, fmt.Sprintf("%d %s", p.n, text.Plural(p.n, p.one, p.many)))
		}
	}
	return strings.Join(parts, ", ")
}

// keysLine fits the key hints to the width, dropping the least important
// first; "q quit" and "? help" always stay.
func (m *Model) keysLine() string {
	keys := []struct {
		key, desc string
		priority  int // lower stays longer
	}{
		{"space", "toggle", 2}, {"a", "all", 7}, {"n", "none", 8}, {"c", "safe", 6}, {"b", "branch", 3},
		{"B", "auto", 5}, {"/", "filter", 4}, {"enter", "review", 1}, {"q", "quit", 0}, {"?", "help", 0},
	}
	for maxPriority := 8; ; maxPriority-- {
		var parts []string
		for _, k := range keys {
			if k.priority <= maxPriority {
				parts = append(parts, styleBold.Render(k.key)+" "+styleDim.Render(k.desc))
			}
		}
		line := " " + strings.Join(parts, "  ")
		if ansi.StringWidth(line) <= m.width || maxPriority == 0 {
			return line
		}
	}
}

func (m *Model) helpLines() []string {
	h := func(s string) string { return " " + styleBold.Render(s) }
	lines := []string{
		h("keys"),
		"   up/down, j/k  move     pgup/pgdn  page     g/G  top/bottom",
		"   space  toggle the row, move down   a  select all shown (incl. AT RISK)",
		"   n      select none (all rows, also those hidden by the filter)",
		"   c      select only the SAFE rows shown",
		"   b      toggle deleting the row's branch, any tier (also selects the row)",
		"   B      auto branches on/off: while on, selecting a row also deletes its branch",
		"          unless it is pushed (b only). Off clears every branch flag.",
		"   /      filter by repo, worktree or branch; esc clears",
		"   enter  review the plan and confirm   q  quit without changing anything",
		"",
		h("STATUS"),
		"   " + styleSafe.Render("clean") + "      SAFE: nothing beyond ignored build output would be lost",
		"   " + styleAtRisk.Render("DIRTY") + "      2S staged, 3M modified, 1? untracked entries, 1U conflicted",
		"   " + styleAtRisk.Render("LOST") + "       DETACHED +N LOST: commits only this detached HEAD has",
		"   " + styleAtRisk.Render("REBASING") + "   or MERGING, CHERRY-PICKING, REVERTING, BISECTING",
		"   " + styleAtRisk.Render("HIDDEN") + "     edits git status can't see (assume-unchanged / skip-worktree)",
		"   " + styleAtRisk.Render("IN USE") + "     processes with their cwd inside (the detail pane lists them)",
		"   " + styleNote.Render("+N ignored") + " ignored files (eg. .env) or dirs not on the build-output",
		"              allowlist (eg. data/): not at risk, but the worktree goes via the Trash",
		"   " + styleDim.Render("[-]") + "        blocked: current, locked, missing, moved, broken, submodules,",
		"              nested, error (the detail pane says how to unblock)",
		"",
		h("BRANCH ACTION"),
		"   delete = will be deleted, -- = deletable but not flagged, keep = never deleted",
		"   merged         tip is in a default branch (origin/HEAD target, local main, ...)",
		"   contained      tip is in another local branch or tag",
		"   pointer        only in remote-tracking refs, never committed to here (PR review)",
		"   squash-merged  unique commits, but their diff is in a default branch",
		"   pushed         only in remote-tracking refs, committed to here: b + typed yes",
		"",
		h("IDLE") + "  time since the last sign of use: HEAD commit, HEAD reflog, changed files",
	}
	if len(m.cfg.Warnings) > 0 {
		lines = append(lines, "", h("warnings"))
		for _, w := range m.cfg.Warnings {
			lines = append(lines, "   "+styleHint.Render(text.Printable(w)))
		}
	}
	if s := m.cfg.Skipped; m.cfg.DirMode && skippedText(s) != "" {
		lines = append(lines, "", h("not listed")+"  "+skippedText(s))
		for _, p := range s.ClonePaths {
			lines = append(lines, "   clone  "+styleDim.Render(text.Printable(text.Tilde(p))))
		}
	}
	return lines
}

func (m *Model) reviewView() string {
	content := m.reviewLines()
	h := m.reviewHeight()
	var lines []string
	for i := m.reviewOffset; i < min(m.reviewOffset+h, len(content)); i++ {
		lines = append(lines, content[i])
	}
	for len(lines) < h {
		lines = append(lines, "")
	}

	sep := strings.Repeat("─", m.width)
	if len(content) > h {
		pos := fmt.Sprintf(" %d-%d of %d  (up/down pgup/pgdn scroll) ", m.reviewOffset+1, min(m.reviewOffset+h, len(content)), len(content))
		sep = "──" + pos + strings.Repeat("─", max(0, m.width-2-ansi.StringWidth(pos)))
	}
	lines = append(lines, styleDim.Render(truncEnd(sep, m.width)))

	if m.plan.NeedsTypedYes() {
		lines = append(lines,
			truncEnd(fmt.Sprintf(` Type %s + enter to confirm (%s)  ·  esc back`, styleAtRisk.Render(`"yes"`), m.typedYesReason()), m.width),
			truncEnd(" > "+text.Printable(m.typed)+"█  "+styleRiskText.Render(m.hint), m.width))
	} else {
		lines = append(lines,
			truncEnd(" Press "+styleBold.Render("y")+" to confirm  ·  esc back", m.width),
			truncEnd(" "+styleHint.Render(m.hint), m.width))
	}
	return strings.Join(lines, "\n")
}

func (m *Model) typedYesReason() string {
	pushed := slices.ContainsFunc(m.plan.Branches, plan.BranchAction.NeedsTypedYes)
	switch {
	case len(m.plan.AtRisk) > 0 && pushed:
		return "at-risk items and a pushed branch selected"
	case pushed:
		return "a pushed, unmerged branch selected"
	}
	return "at-risk items selected"
}

// reviewLines is the scrollable plan: at-risk items with what they lose,
// then safe items, then branches deleted and kept, then anything dropped.
func (m *Model) reviewLines() []string {
	p := m.plan
	if p == nil {
		return nil
	}
	var lines []string
	add := func(s string) { lines = append(lines, truncEnd(s, m.width)) }

	items := p.Items()
	nameW, repoW := 0, 0
	for _, it := range items {
		nameW = max(nameW, ansi.StringWidth(text.Printable(filepath.Base(it.W.Path))))
		repoW = max(repoW, ansi.StringWidth(text.Printable(it.W.Repo.Name)))
	}
	nameW = min(nameW, max(16, m.width/3))
	repoW = min(repoW, max(8, m.width/6))
	if !m.cfg.DirMode {
		repoW = 0
	}
	// addItem prints the name (and repo), then each text wrapped beside it
	// with a hanging indent, so long file lists stay readable.
	addItem := func(it plan.Item, nameStyle lipgloss.Style, texts []string, textStyle lipgloss.Style) {
		name := pad(elideMiddle(text.Printable(filepath.Base(it.W.Path)), nameW), nameW)
		prefix := "    " + nameStyle.Render(name) + "  "
		indent := 4 + nameW + 2
		if repoW > 0 {
			prefix += styleDim.Render(pad(elideMiddle(text.Printable(it.W.Repo.Name), repoW), repoW)) + "  "
			indent += repoW + 2
		}
		if len(texts) == 0 {
			add(strings.TrimRight(prefix, " "))
			return
		}
		for _, t := range texts {
			for _, wl := range strings.Split(ansi.Wrap(text.Printable(t), max(20, m.width-indent), ", "), "\n") {
				add(prefix + textStyle.Render(wl))
				prefix = strings.Repeat(" ", indent)
			}
		}
	}
	trash := text.Printable(text.Tilde(m.cfg.TrashDir))

	add(styleBold.Render(fmt.Sprintf("REMOVE %d %s", len(items), text.Plural(len(items), "worktree", "worktrees"))))
	if len(p.AtRisk) > 0 {
		dest := "moved to " + trash
		if p.NoTrash {
			dest = styleAtRisk.Render("force-removed (--no-trash), NOT recoverable")
		}
		add("  " + styleAtRisk.Render("AT RISK") + " -> " + dest)
		for _, it := range p.AtRisk {
			texts := it.Reasons()
			lost := m.cfg.Lost[it.W.ID]
			for _, c := range lost[:min(len(lost), maxReviewLost)] {
				texts = append(texts, "LOST "+commitLine(c))
			}
			if n := len(lost) - maxReviewLost; n > 0 {
				texts = append(texts, fmt.Sprintf("LOST +%d more (all are recorded in the log)", n))
			}
			if len(it.W.ReflogOnly) > 0 {
				texts = append(texts, "note: "+reflogNote(it.W.ReflogOnly))
			}
			addItem(it, styleAtRisk, texts, styleRiskText)
		}
	}
	if len(p.Safe) > 0 {
		add("  " + styleSafe.Render("safe"))
		for _, it := range p.Safe {
			var notes []string
			for _, r := range it.Reasons() {
				if it.Trash {
					notes = append(notes, "("+r+" -> moved to the Trash)")
				} else {
					notes = append(notes, "("+r+" -> deleted)")
				}
			}
			if len(it.W.ReflogOnly) > 0 {
				notes = append(notes, "(note: "+reflogNote(it.W.ReflogOnly)+")")
			}
			addItem(it, lipgloss.NewStyle(), notes, styleNote)
		}
	}

	if n := len(p.Branches); n > 0 {
		add(styleBold.Render(fmt.Sprintf("DELETE %d %s", n, text.Plural(n, "branch", "branches"))))
		var rows [][]string
		for _, b := range p.Branches {
			marker := ""
			if b.NeedsTypedYes() {
				marker = styleAtRisk.Render("<- typed yes required")
			}
			rows = append(rows, []string{text.Printable(b.Name), m.repoCell(b.Repo), text.Printable(b.Label), marker})
		}
		m.addTable(add, rows)
	}

	dropped := slices.Clone(p.Dropped)
	var kept [][]string
	for _, it := range items {
		if it.DeleteBranch || it.W.Branch.Name == "" {
			continue
		}
		b := it.W.Branch
		reason := text.Printable(b.Label())
		if b.Candidate() {
			reason += " -- not flagged (b on the row deletes it)"
		}
		if i := slices.IndexFunc(dropped, func(d plan.Dropped) bool { return d.Kind == "branch" && d.Name == b.Name }); i >= 0 {
			reason = "not deleted: " + text.Printable(dropped[i].Reason)
			dropped = slices.Delete(dropped, i, i+1)
		}
		kept = append(kept, []string{text.Printable(b.Name), m.repoCell(it.W.Repo), reason, ""})
	}
	if len(kept) > 0 {
		add(styleBold.Render(fmt.Sprintf("KEEP %d %s", len(kept), text.Plural(len(kept), "branch", "branches"))))
		m.addTable(add, kept)
	}

	if n := len(dropped); n > 0 {
		add(styleBold.Render(fmt.Sprintf("NOT INCLUDED %d", n)))
		for _, d := range dropped {
			add(text.Printable(fmt.Sprintf("  %s %s: %s", d.Kind, text.Tilde(d.Name), d.Reason)))
		}
	}
	return lines
}

func (m *Model) repoCell(r *scan.Repo) string {
	if !m.cfg.DirMode {
		return ""
	}
	return text.Printable(r.Name)
}

// addTable adds rows of [name, repo, label, marker] with aligned columns.
// The label is padded (and cut) only when a marker follows it.
func (m *Model) addTable(add func(string), rows [][]string) {
	nameW, repoW, labelW, markers := 0, 0, 0, false
	for _, r := range rows {
		nameW = max(nameW, ansi.StringWidth(r[0]))
		repoW = max(repoW, ansi.StringWidth(r[1]))
		labelW = max(labelW, ansi.StringWidth(r[2]))
		markers = markers || r[3] != ""
	}
	nameW = min(nameW, max(16, m.width/3))
	repoW = min(repoW, max(8, m.width/6))
	labelW = min(labelW, max(20, m.width/2))
	for _, r := range rows {
		line := "  " + pad(elideMiddle(r[0], nameW), nameW) + "  "
		if repoW > 0 {
			line += styleDim.Render(pad(elideMiddle(r[1], repoW), repoW)) + "  "
		}
		if markers {
			line += pad(truncEnd(r[2], labelW), labelW) + "  " + r[3]
		} else {
			line += r[2]
		}
		add(strings.TrimRight(line, " "))
	}
}
