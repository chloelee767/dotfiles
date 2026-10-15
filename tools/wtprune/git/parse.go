package git

import (
	"fmt"
	"strconv"
	"strings"
)

// WorktreeEntry is one record of `git worktree list --porcelain -z`.
type WorktreeEntry struct {
	Path       string
	Head       string
	Branch     string // full ref, eg. refs/heads/feat; empty when detached
	Detached   bool
	Bare       bool
	Locked     bool
	LockReason string
	Prunable   bool
}

// ParseWorktreeList parses `git worktree list --porcelain -z`: NUL-terminated
// attribute lines, with an empty field ending each record.
func ParseWorktreeList(out string) []WorktreeEntry {
	var entries []WorktreeEntry
	var cur *WorktreeEntry
	for _, field := range strings.Split(out, "\x00") {
		if field == "" {
			cur = nil
			continue
		}
		key, value, _ := strings.Cut(field, " ")
		if key == "worktree" {
			entries = append(entries, WorktreeEntry{Path: value})
			cur = &entries[len(entries)-1]
			continue
		}
		if cur == nil {
			continue
		}
		switch key {
		case "HEAD":
			cur.Head = value
		case "branch":
			cur.Branch = value
		case "detached":
			cur.Detached = true
		case "bare":
			cur.Bare = true
		case "locked":
			cur.Locked = true
			cur.LockReason = value
		case "prunable":
			cur.Prunable = true
		}
	}
	return entries
}

// Entry kinds of `git status --porcelain=v2`.
const (
	KindChanged    = '1'
	KindRenamed    = '2'
	KindConflicted = 'u'
	KindUntracked  = '?'
	KindIgnored    = '!'
)

// StatusEntry is one non-header line of `git status --porcelain=v2 -z`.
// XY is "??" for untracked and "!!" for ignored entries. Untracked and
// ignored directories that git collapsed keep their trailing "/".
type StatusEntry struct {
	Kind     byte
	XY       string
	Path     string
	OrigPath string // renames/copies only
}

// Status is the parsed output of `git status --porcelain=v2 --branch -z`.
type Status struct {
	OID      string // "(initial)" on an unborn branch
	Upstream string
	Ahead    int
	Behind   int
	HasAB    bool
	Entries  []StatusEntry
}

// ParseStatus parses `git status --porcelain=v2 --branch -z`.
func ParseStatus(out string) (Status, error) {
	var st Status
	fields := strings.Split(out, "\x00")
	for i := 0; i < len(fields); i++ {
		line := fields[i]
		if line == "" {
			continue
		}
		if header, ok := strings.CutPrefix(line, "# "); ok {
			if err := st.parseHeader(header); err != nil {
				return st, err
			}
			continue
		}
		e := StatusEntry{Kind: line[0]}
		var parts []string
		switch e.Kind {
		case KindChanged:
			// 1 XY sub mH mI mW hH hI path
			parts = strings.SplitN(line, " ", 9)
		case KindRenamed:
			// 2 XY sub mH mI mW hH hI Xscore path, then NUL origPath
			parts = strings.SplitN(line, " ", 10)
			if i+1 >= len(fields) {
				return st, fmt.Errorf("status: rename without original path: %q", line)
			}
			i++
			e.OrigPath = fields[i]
		case KindConflicted:
			// u XY sub m1 m2 m3 mW h1 h2 h3 path
			parts = strings.SplitN(line, " ", 11)
		case KindUntracked, KindIgnored:
			e.XY = strings.Repeat(string(e.Kind), 2)
			e.Path = line[2:]
			st.Entries = append(st.Entries, e)
			continue
		default:
			return st, fmt.Errorf("status: unknown entry %q", line)
		}
		if len(parts) < 3 {
			return st, fmt.Errorf("status: short entry %q", line)
		}
		e.XY = parts[1]
		e.Path = parts[len(parts)-1]
		st.Entries = append(st.Entries, e)
	}
	return st, nil
}

func (st *Status) parseHeader(header string) error {
	key, value, _ := strings.Cut(header, " ")
	switch key {
	case "branch.oid":
		st.OID = value
	case "branch.upstream":
		st.Upstream = value
	case "branch.ab":
		a, b, ok := strings.Cut(value, " ")
		ahead, err1 := strconv.Atoi(strings.TrimPrefix(a, "+"))
		behind, err2 := strconv.Atoi(strings.TrimPrefix(b, "-"))
		if !ok || err1 != nil || err2 != nil {
			return fmt.Errorf("status: bad branch.ab %q", value)
		}
		st.Ahead, st.Behind, st.HasAB = ahead, behind, true
	}
	return nil
}

// IndexEntry is one record of `git ls-files -v -s -z`.
type IndexEntry struct {
	Tag  byte // H cached, S skip-worktree, lowercase: also assume-unchanged
	Mode string
	SHA  string
	Path string
}

// AssumeUnchanged reports a lowercase tag: edits are invisible to status.
func (e IndexEntry) AssumeUnchanged() bool { return e.Tag >= 'a' && e.Tag <= 'z' }

// SkipWorktree reports an S/s tag: status ignores the file entirely (sparse
// checkout, or `update-index --skip-worktree`).
func (e IndexEntry) SkipWorktree() bool { return e.Tag == 'S' || e.Tag == 's' }

// Gitlink reports a submodule entry.
func (e IndexEntry) Gitlink() bool { return e.Mode == "160000" }

// ParseIndex parses `git ls-files -v -s -z`: "<tag> <mode> <sha> <stage>\t<path>".
func ParseIndex(out string) []IndexEntry {
	var entries []IndexEntry
	for _, field := range strings.Split(out, "\x00") {
		meta, path, ok := strings.Cut(field, "\t")
		if !ok {
			continue
		}
		parts := strings.Fields(meta)
		if len(parts) != 4 || len(parts[0]) != 1 {
			continue
		}
		entries = append(entries, IndexEntry{Tag: parts[0][0], Mode: parts[1], SHA: parts[2], Path: path})
	}
	return entries
}
