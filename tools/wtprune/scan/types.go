package scan

import (
	"fmt"
	"slices"
	"strings"
	"time"
)

// State is why a row is blocked; StateOK rows are selectable.
type State string

const (
	StateOK         State = ""
	StateCurrent    State = "current"
	StateLocked     State = "locked"
	StateMissing    State = "missing"
	StateMoved      State = "moved"
	StateBroken     State = "broken"
	StateSubmodules State = "submodules"
	StateNested     State = "nested"
	StateError      State = "error" // inspection failed; never act on what we could not see
)

// Repo is one repository, keyed by its canonical common dir.
type Repo struct {
	Name      string
	CommonDir string // canonical
	MainPath  string // main worktree; empty for a bare repo

	registered []registration
	protected  map[string]bool // branch short names
	// defaultBranches are full refs: each refs/remotes/*/HEAD target, then
	// the local protected branches that exist (main, master, ...).
	defaultBranches []string
}

// Change is one tracked/untracked/conflicted status entry.
type Change struct {
	Code     string // porcelain v1 style without padding: M, A, D, R, MM, UU, ??
	Path     string // untracked dirs keep their trailing "/"
	OrigPath string // renames only
}

func (c Change) String() string { return c.Code + " " + c.Path }

// Process is a process whose cwd was seen by the process lister.
type Process struct {
	PID     int
	Command string
	Cwd     string
}

// Commit is a commit as shown to the user; Time may be zero.
type Commit struct {
	SHA     string
	Subject string
	Time    time.Time
}

// Tier classifies a branch that is a deletion candidate.
type Tier string

const (
	TierNone      Tier = ""          // not a candidate: keep
	TierMerged    Tier = "merged"    // tip is in a default branch (origin/master, local main, ...)
	TierContained Tier = "contained" // tip is in another local branch or a tag
	TierPointer   Tier = "pointer"   // tip only in remote-tracking refs, never authored here
	TierSquashed  Tier = "squashed"  // has unique commits, but their diff was squash-merged into a default branch
	TierPushed    Tier = "pushed"    // tip only in remote-tracking refs, authored here
)

// Branch is the branch a worktree has checked out, plus its deletion verdict.
type Branch struct {
	Name         string // short name; empty when detached
	Tip          string
	Upstream     string // eg. origin/feat-x
	UpstreamGone bool   // Upstream is configured but its ref no longer exists
	Ahead        int
	Behind       int

	Tier        Tier
	KeepReason  string // set when Tier is TierNone: "2 unique", "protected", "detached", "in use elsewhere", ...
	ContainedIn string // short ref name containing the tip, or the default branch it was squash-merged into
	SameTip     bool   // ContainedIn points at exactly Tip

	SquashCommit string // TierSquashed: the default-branch commit with the same patch-id
	SquashRef    string // TierSquashed: full ref of that default branch
}

// Candidate reports whether the branch may be deleted at all.
func (b Branch) Candidate() bool { return b.Tier != TierNone }

// Bulk reports whether `B` / --delete-branches may select it; pushed
// branches need an individual `b`.
func (b Branch) Bulk() bool { return b.Candidate() && b.Tier != TierPushed }

// Label is the BRANCH ACTION text after "delete"/"keep", eg.
// "pointer (= origin/feat-x)", "merged (in master)", "2 unique". Empty for
// detached rows: there is no branch to keep.
func (b Branch) Label() string {
	switch b.Tier {
	case TierMerged:
		return fmt.Sprintf("merged (in %s)", b.ContainedIn)
	case TierContained:
		return fmt.Sprintf("contained (in %s)", b.ContainedIn)
	case TierSquashed:
		return fmt.Sprintf("squash-merged (into %s)", b.ContainedIn)
	case TierPointer:
		if b.SameTip {
			return fmt.Sprintf("pointer (= %s)", b.ContainedIn)
		}
		return fmt.Sprintf("pointer (in %s)", b.ContainedIn)
	case TierPushed:
		return "pushed, not merged"
	}
	if b.KeepReason == keepDetached {
		return ""
	}
	return b.KeepReason
}

const keepDetached = "detached"

// IgnoredEntry is one ignored path. Dirs end in "/". Regenerable entries
// (.DS_Store, and dirs on the build-artifact allowlist like node_modules/)
// may be deleted without going via the Trash.
type IgnoredEntry struct {
	Path        string
	Dir         bool
	Regenerable bool
}

// regenerableDirs are basenames of ignored dirs that hold only build output
// or caches, so a plain removal may delete them.
var regenerableDirs = map[string]bool{
	"node_modules": true, "build": true, "dist": true, "out": true, "target": true, "bin": true, "obj": true,
	".gradle": true, ".idea": true, "vendor": true, "__pycache__": true, ".venv": true, "venv": true,
	".pytest_cache": true, ".mypy_cache": true, ".ruff_cache": true, ".next": true, ".nuxt": true, ".turbo": true,
	"coverage": true, "DerivedData": true, "Pods": true, ".build": true, ".terraform": true, ".dart_tool": true,
	".angular": true, ".parcel-cache": true, ".svelte-kit": true, "tmp": true,
}

// Worktree is one row: a linked worktree and everything known about it.
type Worktree struct {
	ID       string // canonical path; stable key for selection
	Path     string // as registered with git (the found dir for moved/broken)
	Repo     *Repo
	AdminDir string // <common>/worktrees/<id>
	Head     string
	Detached bool
	Branch   Branch

	Locked     bool
	LockReason string

	State State
	Hint  string // for blocked rows: how to unblock, or what went wrong

	Changes      []Change
	Staged       int
	Modified     int
	Untracked    int
	Conflicted   int
	Ignored      []IgnoredEntry // every ignored entry
	IgnoredFiles []string       // ignored non-dirs except .DS_Store
	IgnoredDirs  []string       // ignored dirs not on the regenerable allowlist, eg. data/
	Hidden       []string       // edits invisible to status: assume-unchanged, or skip-worktree files that differ
	InProgress   string         // REBASING, MERGING, CHERRY-PICKING, REVERTING, BISECTING
	DetachedLost int            // commits only reachable from this detached HEAD
	ReflogOnly   []Commit       // commits only reachable from this worktree's HEAD or branch reflog (a note, not a risk)
	InUse        []Process
	Nested       []string // worktrees/repos inside this one

	LastCommit Commit
	LastActive time.Time
	Idle       time.Duration
}

// Selectable reports whether the row can be selected for removal.
func (w *Worktree) Selectable() bool { return w.State == StateOK }

// Dirty reports tracked, untracked or conflicted changes.
func (w *Worktree) Dirty() bool {
	return w.Staged+w.Modified+w.Untracked+w.Conflicted > 0
}

// AtRisk reports whether removing the row would lose something beyond
// ignored build artifacts.
func (w *Worktree) AtRisk() bool {
	return w.Dirty() || w.DetachedLost > 0 || w.InProgress != "" || len(w.Hidden) > 0 || len(w.InUse) > 0
}

// Safe is a selectable row that is not at risk (`c` / --select clean).
func (w *Worktree) Safe() bool { return w.Selectable() && !w.AtRisk() }

// NeedsTrash reports whether removal goes via the Trash: at risk, or has
// ignored files (eg. .env) or non-regenerable ignored dirs that a plain
// removal would silently delete.
func (w *Worktree) NeedsTrash() bool { return w.AtRisk() || w.IgnoredCount() > 0 }

// IgnoredCount is the "+N ignored" note: ignored files plus non-regenerable
// ignored dirs.
func (w *Worktree) IgnoredCount() int { return len(w.IgnoredFiles) + len(w.IgnoredDirs) }

// RiskFlags lists the at-risk markers, eg. ["DIRTY 2S 3M 1?", "IN USE zsh"].
func (w *Worktree) RiskFlags() []string {
	var flags []string
	if w.Dirty() {
		var parts []string
		for _, p := range []struct {
			n      int
			suffix string
		}{{w.Staged, "S"}, {w.Modified, "M"}, {w.Untracked, "?"}, {w.Conflicted, "U"}} {
			if p.n > 0 {
				parts = append(parts, fmt.Sprintf("%d%s", p.n, p.suffix))
			}
		}
		flags = append(flags, "DIRTY "+strings.Join(parts, " "))
	}
	if w.DetachedLost > 0 {
		flags = append(flags, fmt.Sprintf("DETACHED +%d LOST", w.DetachedLost))
	}
	if w.InProgress != "" {
		flags = append(flags, w.InProgress)
	}
	if len(w.Hidden) > 0 {
		flags = append(flags, fmt.Sprintf("HIDDEN %d", len(w.Hidden)))
	}
	if len(w.InUse) > 0 {
		flags = append(flags, "IN USE "+strings.Join(w.inUseCommands(), ","))
	}
	return flags
}

func (w *Worktree) inUseCommands() []string {
	var names []string
	for _, p := range w.InUse {
		if !slices.Contains(names, p.Command) {
			names = append(names, p.Command)
		}
	}
	return names
}

// StatusText is the STATUS column: the blocked state, the risk flags, or
// "clean", plus "+N ignored" (see IgnoredCount).
func (w *Worktree) StatusText() string {
	if !w.Selectable() {
		return string(w.State)
	}
	text := "clean"
	if flags := w.RiskFlags(); len(flags) > 0 {
		text = strings.Join(flags, " ")
	}
	if n := w.IgnoredCount(); n > 0 {
		text += fmt.Sprintf(" +%d ignored", n)
	}
	return text
}

// Fingerprint captures what Execute must see unchanged since the scan:
// HEAD, the changed, hidden and kept-ignored paths, and any in-progress
// operation.
func (w *Worktree) Fingerprint() string {
	var b strings.Builder
	fmt.Fprintf(&b, "head=%s\nbranch=%s\nop=%s\n", w.Head, w.Branch.Name, w.InProgress)
	for _, c := range w.Changes {
		fmt.Fprintf(&b, "change=%s\x00%s\x00%s\n", c.Code, c.Path, c.OrigPath)
	}
	for _, p := range slices.Concat(w.IgnoredFiles, w.IgnoredDirs) {
		fmt.Fprintf(&b, "ignored=%s\n", p)
	}
	for _, p := range w.Hidden {
		fmt.Fprintf(&b, "hidden=%s\n", p)
	}
	return b.String()
}

// IdleText is the IDLE column, or "--" when nothing dated the row.
func (w *Worktree) IdleText() string {
	if w.LastActive.IsZero() {
		return "--"
	}
	return FormatIdle(w.Idle)
}

// FormatIdle renders an idle duration as 5m, 4h, 23d.
func FormatIdle(d time.Duration) string {
	switch {
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d.Minutes()))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh", int(d.Hours()))
	}
	return fmt.Sprintf("%dd", int(d.Hours()/24))
}
