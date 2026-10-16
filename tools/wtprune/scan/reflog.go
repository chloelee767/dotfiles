package scan

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
)

// reflogEntry is one line of a reflog: the commit the ref moved to, and why.
type reflogEntry struct {
	newOID string
	msg    string
}

// readReflog returns a ref's reflog entries, newest first, straight from
// the files ref backend: a `git reflog` per worktree and branch adds up.
// ref is "HEAD" (the worktree's own) or a full refs/heads/ name. ok is
// false when git must be asked instead: a reftable repo, or no readable
// file.
func (w *Worktree) readReflog(ref string) (entries []reflogEntry, ok bool) {
	if w.Repo.reftable {
		return nil, false
	}
	path := filepath.Join(w.Repo.CommonDir, "logs", filepath.FromSlash(ref))
	if ref == "HEAD" {
		path = filepath.Join(w.AdminDir, "logs", "HEAD")
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, false
	}
	lines := strings.Split(string(data), "\n")
	// The last element is "" after a final newline, else a line that is
	// still being written: git ignores it, so do we.
	for _, line := range lines[:len(lines)-1] {
		if e, ok := parseReflogLine(line); ok && strings.Trim(e.newOID, "0") != "" { // all zeros: the ref was deleted
			entries = append(entries, e)
		}
	}
	slices.Reverse(entries)
	return entries, true
}

// parseReflogLine parses `<old> <new> <name> <<email>> <time> <tz>[\t<msg>]`
// as git does: the identity ends at its first '>', so a name may hold
// spaces or tabs. Malformed lines are skipped, as git skips them.
func parseReflogLine(line string) (reflogEntry, bool) {
	old, rest, ok1 := strings.Cut(line, " ")
	newOID, rest, ok2 := strings.Cut(rest, " ")
	end := strings.IndexByte(rest, '>')
	if !ok1 || !ok2 || !isHexOID(old) || !isHexOID(newOID) || end < 0 || !strings.HasPrefix(rest[end+1:], " ") {
		return reflogEntry{}, false
	}
	timestamp, rest, ok := strings.Cut(rest[end+2:], " ")
	if !ok || strings.Trim(timestamp, "0") == "" || strings.Trim(timestamp, "0123456789") != "" ||
		len(rest) < 5 || (rest[0] != '+' && rest[0] != '-') || strings.Trim(rest[1:5], "0123456789") != "" {
		return reflogEntry{}, false
	}
	return reflogEntry{newOID: newOID, msg: strings.TrimPrefix(rest[5:], "\t")}, true
}

func isHexOID(s string) bool {
	return (len(s) == 40 || len(s) == 64) && strings.Trim(s, "0123456789abcdef") == ""
}
