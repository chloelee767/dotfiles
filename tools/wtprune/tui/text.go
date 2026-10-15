package tui

import (
	"strings"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/x/ansi"
)

const ellipsis = "…"

var (
	styleBold     = lipgloss.NewStyle().Bold(true)
	styleDim      = lipgloss.NewStyle().Foreground(lipgloss.Color("244"))
	styleAtRisk   = lipgloss.NewStyle().Foreground(lipgloss.Color("1")).Bold(true)
	styleRiskText = lipgloss.NewStyle().Foreground(lipgloss.Color("1"))
	styleSafe     = lipgloss.NewStyle().Foreground(lipgloss.Color("2"))
	styleNote     = lipgloss.NewStyle().Foreground(lipgloss.Color("3"))
	styleDelete   = lipgloss.NewStyle().Foreground(lipgloss.Color("5")).Bold(true)
	styleHint     = lipgloss.NewStyle().Foreground(lipgloss.Color("3"))
	styleTitle    = lipgloss.NewStyle().Bold(true).Underline(true)
)

// truncEnd cuts s to width cells, ending in an ellipsis when cut.
func truncEnd(s string, width int) string {
	if width <= 0 {
		return ""
	}
	return ansi.Truncate(s, width, ellipsis)
}

// elideMiddle cuts unstyled s to at most width cells by replacing its middle
// with an ellipsis, keeping both ends (eg. a repo prefix and a ticket suffix).
func elideMiddle(s string, width int) string {
	if ansi.StringWidth(s) <= width {
		return s
	}
	if width <= 2 {
		return truncEnd(s, width)
	}
	tailW := (width - 1) / 2
	headW := width - 1 - tailW
	return ansi.Truncate(s, headW, "") + ellipsis + tail(s, tailW)
}

// tail is the longest suffix of s at most width cells wide. A wide
// character straddling the cut is dropped rather than overflowing.
func tail(s string, width int) string {
	r := []rune(s)
	start := len(r)
	for start > 0 && ansi.StringWidth(string(r[start-1:])) <= width {
		start--
	}
	return string(r[start:])
}

// pad right-pads s with spaces to width cells.
func pad(s string, width int) string {
	if gap := width - ansi.StringWidth(s); gap > 0 {
		return s + strings.Repeat(" ", gap)
	}
	return s
}

func padLeft(s string, width int) string {
	if gap := width - ansi.StringWidth(s); gap > 0 {
		return strings.Repeat(" ", gap) + s
	}
	return s
}

// fitLine cuts an already styled line to the terminal width.
func fitLine(s string, width int) string {
	return ansi.Truncate(s, width, "")
}
