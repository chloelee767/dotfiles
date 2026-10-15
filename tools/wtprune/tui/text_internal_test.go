package tui

import (
	"testing"

	"github.com/charmbracelet/x/ansi"
)

func TestElideMiddleNeverOverflows(t *testing.T) {
	for _, s := range []string{"日本語のブランチ名前とても長い", "a日本語b日本語c日本語", "🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲", "plain-ascii-name-that-is-long"} {
		for width := 1; width <= 12; width++ {
			if got := elideMiddle(s, width); ansi.StringWidth(got) > width {
				t.Errorf("elideMiddle(%q, %d) = %q, %d cells wide", s, width, got, ansi.StringWidth(got))
			}
		}
	}
	if got := elideMiddle("repo-review-818", 11); got != "repo-…w-818" {
		t.Errorf("elideMiddle keeps both ends: got %q", got)
	}
}
