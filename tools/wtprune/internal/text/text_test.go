package text_test

import (
	"testing"

	"wtprune/internal/text"
)

func TestPrintable(t *testing.T) {
	for in, want := range map[string]string{
		"feat/x":         "feat/x",
		"日本語 🌲":          "日本語 🌲",
		"a\nb":           `a\nb`,
		"tab\there":      `tab\there`,
		"\x1b[31mred":    `\x1b[31mred`,
		"del\x7f":        `del\x7f`,
		"c1\u009bcursor": `c1\u009bcursor`,
	} {
		if got := text.Printable(in); got != want {
			t.Errorf("Printable(%q) = %q, want %q", in, got, want)
		}
	}
}
