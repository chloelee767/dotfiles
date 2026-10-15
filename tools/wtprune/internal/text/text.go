// Package text holds small display helpers shared by the CLI, TUI and plan
// output.
package text

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// Tilde abbreviates the home dir prefix of p to ~.
func Tilde(p string) string {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return p
	}
	if rest, ok := strings.CutPrefix(p, home+string(filepath.Separator)); ok {
		return "~/" + rest
	}
	return p
}

// Short abbreviates a commit sha to 7 characters.
func Short(sha string) string {
	if len(sha) > 7 {
		return sha[:7]
	}
	return sha
}

// Plural picks one or many by n.
func Plural(n int, one, many string) string {
	if n == 1 {
		return one
	}
	return many
}

// Printable escapes control characters (eg. a newline or ESC in a file or
// branch name) Go-style, so they can't break or restyle a terminal line.
func Printable(s string) string {
	if !strings.ContainsFunc(s, isControl) {
		return s
	}
	var b strings.Builder
	for _, r := range s {
		if isControl(r) {
			q := strconv.QuoteRune(r)
			b.WriteString(q[1 : len(q)-1])
		} else {
			b.WriteRune(r)
		}
	}
	return b.String()
}

func isControl(r rune) bool { return r < 0x20 || r == 0x7f || (r >= 0x80 && r < 0xa0) }
