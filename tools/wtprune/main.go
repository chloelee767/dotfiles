// wtprune previews and prunes old git worktrees of one repo or of a folder
// across repos, optionally deleting their useless branches. At-risk
// worktrees go to the Trash first and every action is logged for recovery.
package main

import (
	"cmp"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"text/tabwriter"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/x/term"

	"wtprune/git"
	"wtprune/internal/text"
	"wtprune/plan"
	"wtprune/scan"
	"wtprune/tui"
)

const usage = `Usage: wtprune [--repo PATH | --dir PATH] [options]

Preview, select and remove linked git worktrees; optionally delete their
useless branches. At-risk worktrees are moved to ~/.Trash first.

Target (default: --repo .):
  --repo PATH        all linked worktrees of the repo containing PATH (any worktree of it works)
  --dir PATH         every linked worktree found under PATH, across repos
  --depth N          max directory depth for --dir discovery (default 3)

Filters / initial state:
  --older-than DUR   only show worktrees idle at least DUR (Go durations plus d/w: 36h, 14d, 2w);
                     rows whose idle time is unknown are always shown
  --select none|clean
                     initial selection (default none). "clean" = SAFE rows only
  --delete-branches  start with B on: selecting a row also deletes its branch if it is
                     merged, contained, a pointer or squash-merged (not pushed)

Execution:
  --no-trash         hard-delete at-risk worktrees instead of moving them to ~/.Trash first

Output:
  --list             print the table and exit (no TUI, no changes)

--list is implied when stdin or stdout is not a terminal.

Environment (testing knobs):
  WTPRUNE_TRASH_DIR  Trash dir for at-risk worktrees (default ~/.Trash)
  WTPRUNE_LOG        action log path (default $XDG_STATE_HOME/wtprune/log or ~/.local/state/wtprune/log)

Exit codes: 0 ok (incl. user quit), 1 some actions failed or were interrupted,
2 usage/discovery error, 130 interrupted during the scan.
`

const (
	exitOK          = 0
	exitFailed      = 1
	exitUsage       = 2
	exitInterrupted = 130
)

// maxLostSubjects caps the commits whose subject is looked up for the
// review screen; the rest are listed by count.
const maxLostSubjects = 50

type streams struct {
	out, err    io.Writer
	interactive bool // stdin and stdout are terminals
	progress    bool // stderr is a terminal
}

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	code := run(ctx, stop, os.Args[1:], streams{
		out:         os.Stdout,
		err:         os.Stderr,
		interactive: isTerminal(os.Stdin) && isTerminal(os.Stdout),
		progress:    isTerminal(os.Stderr),
	})
	stop()
	os.Exit(code)
}

// run is the whole CLI. stop restores default signal handling: called once
// execution starts so a second Ctrl-C kills the process.
func run(ctx context.Context, stop func(), args []string, s streams) int {
	fs := flag.NewFlagSet("wtprune", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	var (
		repo, dir, olderThan, selectMode string
		depth                            int
		deleteBranches, noTrash, list    bool
	)
	fs.StringVar(&repo, "repo", "", "")
	fs.StringVar(&dir, "dir", "", "")
	fs.IntVar(&depth, "depth", 3, "")
	fs.StringVar(&olderThan, "older-than", "", "")
	fs.StringVar(&selectMode, "select", "none", "")
	fs.BoolVar(&deleteBranches, "delete-branches", false, "")
	fs.BoolVar(&noTrash, "no-trash", false, "")
	fs.BoolVar(&list, "list", false, "")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			fmt.Fprint(s.out, usage)
			return exitOK
		}
		return usageErr(s.err, "%v", err)
	}
	if fs.NArg() > 0 {
		return usageErr(s.err, "unexpected argument: %s", fs.Arg(0))
	}
	if repo != "" && dir != "" {
		return usageErr(s.err, "pass at most one of --repo, --dir")
	}
	if repo == "" && dir == "" {
		repo = "."
	}
	if depth < 1 {
		return usageErr(s.err, "--depth must be at least 1")
	}
	if selectMode != "none" && selectMode != "clean" {
		return usageErr(s.err, "--select must be none or clean, got %q", selectMode)
	}
	var minIdle time.Duration
	if olderThan != "" {
		d, err := parseAge(olderThan)
		if err != nil {
			return usageErr(s.err, "--older-than: %v", err)
		}
		minIdle = d
	}
	if !s.interactive {
		list = true
	}

	opts := scan.Options{Repo: repo, Dir: dir, Depth: depth}
	if s.progress {
		opts.Progress = func(done, total int) { fmt.Fprintf(s.err, "\rscanning %d/%d...", done, total) }
	}
	res, err := scan.Scan(ctx, opts)
	if opts.Progress != nil {
		fmt.Fprint(s.err, "\r\033[K")
	}
	if ctx.Err() != nil {
		return exitInterrupted
	}
	if err != nil {
		fmt.Fprintf(s.err, "wtprune: %s\n", text.Printable(err.Error()))
		return exitUsage
	}
	for _, w := range res.Warnings {
		fmt.Fprintf(s.err, "warning: %s\n", text.Printable(w))
	}
	rows := filterByAge(res.Rows, minIdle)

	if list || len(rows) == 0 {
		printList(s.out, res, rows)
		return exitOK
	}

	lost := lostCommits(ctx, rows)
	if ctx.Err() != nil {
		return exitInterrupted
	}
	trashDir := cmp.Or(os.Getenv("WTPRUNE_TRASH_DIR"), plan.DefaultTrashDir())
	logPath := cmp.Or(os.Getenv("WTPRUNE_LOG"), plan.DefaultLogPath())
	model := tui.New(tui.Config{
		Target:         text.Tilde(res.Target),
		DirMode:        res.DirMode,
		Rows:           rows,
		HiddenByAge:    len(res.Rows) - len(rows),
		SelectClean:    selectMode == "clean",
		DeleteBranches: deleteBranches,
		NoTrash:        noTrash,
		TrashDir:       trashDir,
		Lost:           lost,
		Warnings:       res.Warnings,
		Skipped:        res.Skipped,
	})
	if _, err := tea.NewProgram(model, tea.WithAltScreen(), tea.WithContext(ctx)).Run(); err != nil {
		if ctx.Err() != nil {
			return exitOK // interrupted before confirming: nothing was done
		}
		fmt.Fprintf(s.err, "wtprune: %v\n", err)
		return exitFailed
	}
	p := model.ConfirmedPlan()
	if p == nil {
		return exitOK
	}

	// The first Ctrl-C lets the current action finish and stops; restoring
	// the default handler then lets a second one kill the process.
	context.AfterFunc(ctx, stop)
	sum := plan.Execute(ctx, p, s.out, plan.Options{TrashDir: trashDir, LogPath: logPath})
	if sum.Trashed > 0 {
		fmt.Fprintf(s.out, "restore: see \"Recovering\" in tools/wtprune/README.md; log: %s\n", text.Tilde(sum.LogPath))
	}
	return sum.ExitCode()
}

// filterByAge keeps rows idle at least minIdle, and rows whose idle time is
// unknown: hiding them would hide what the tool could not date.
func filterByAge(rows []*scan.Worktree, minIdle time.Duration) []*scan.Worktree {
	var out []*scan.Worktree
	for _, w := range rows {
		if minIdle == 0 || w.LastActive.IsZero() || w.Idle >= minIdle {
			out = append(out, w)
		}
	}
	return out
}

// lostCommits lists, for the review screen, the commits each detached or
// mid-operation row would lose. Failures just leave a row out: the scan's
// counts still mark it at risk.
func lostCommits(ctx context.Context, rows []*scan.Worktree) map[string][]scan.Commit {
	lost := map[string][]scan.Commit{}
	for _, w := range rows {
		if !w.Selectable() || (w.DetachedLost == 0 && w.InProgress == "") {
			continue
		}
		shas, err := scan.LostCommits(ctx, w)
		if err != nil || len(shas) == 0 {
			continue
		}
		subjects := map[string]string{}
		args := append([]string{"log", "--no-walk=unsorted", "--format=%H%x00%s"}, shas[:min(len(shas), maxLostSubjects)]...)
		if out, err := git.Run(ctx, w.Repo.CommonDir, args...); err == nil {
			for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
				if sha, subject, ok := strings.Cut(line, "\x00"); ok {
					subjects[sha] = subject
				}
			}
		}
		for _, sha := range shas {
			lost[w.ID] = append(lost[w.ID], scan.Commit{SHA: sha, Subject: subjects[sha]})
		}
	}
	return lost
}

func printList(out io.Writer, res *scan.Result, rows []*scan.Worktree) {
	fmt.Fprintf(out, "wtprune  %s  ·  %s\n", text.Printable(text.Tilde(res.Target)), countLine(res, rows))
	if len(rows) > 0 {
		fmt.Fprintln(out)
		tw := tabwriter.NewWriter(out, 0, 0, 2, ' ', 0)
		header := "WORKTREE\tBRANCH\tIDLE\tSTATUS\tBRANCH ACTION"
		if res.DirMode {
			header = "REPO\t" + header
		}
		fmt.Fprintln(tw, header)
		for _, w := range rows {
			branch := w.Branch.Name
			if w.Detached {
				branch = "(detached)"
			}
			cells := []string{filepath.Base(w.Path), cmp.Or(branch, "--"), w.IdleText(), w.StatusText(), tui.ActionText(w, false)}
			if res.DirMode {
				cells = append([]string{w.Repo.Name}, cells...)
			}
			for i := range cells {
				cells[i] = text.Printable(cells[i])
			}
			fmt.Fprintln(tw, strings.TrimRight(strings.Join(cells, "\t"), "\t"))
		}
		tw.Flush()
	}

	var blocked []*scan.Worktree
	for _, w := range rows {
		if !w.Selectable() && w.Hint != "" {
			blocked = append(blocked, w)
		}
	}
	if len(blocked) > 0 {
		fmt.Fprintln(out, "\nblocked:")
		for _, w := range blocked {
			fmt.Fprintf(out, "  %-10s %s: %s\n", w.State, text.Printable(text.Tilde(w.Path)), text.Printable(w.Hint))
		}
	}
	if s := res.Skipped; res.DirMode && s.Clones+s.OtherGit+s.NonGit > 0 {
		fmt.Fprintf(out, "\nnot listed: %d %s, %d other .git %s, %d non-git %s\n",
			s.Clones, text.Plural(s.Clones, "clone/main worktree", "clones/main worktrees"),
			s.OtherGit, text.Plural(s.OtherGit, "file", "files"),
			s.NonGit, text.Plural(s.NonGit, "dir", "dirs"))
		for _, p := range s.ClonePaths {
			fmt.Fprintf(out, "  clone  %s\n", text.Printable(text.Tilde(p)))
		}
	}
}

func countLine(res *scan.Result, rows []*scan.Worktree) string {
	if len(res.Rows) == 0 {
		return "no linked worktrees"
	}
	repos := map[string]bool{}
	safe, atRisk, blocked := 0, 0, 0
	for _, w := range rows {
		repos[w.Repo.CommonDir] = true
		switch {
		case !w.Selectable():
			blocked++
		case w.AtRisk():
			atRisk++
		default:
			safe++
		}
	}
	line := fmt.Sprintf("%d %s", len(rows), text.Plural(len(rows), "worktree", "worktrees"))
	if res.DirMode {
		line += fmt.Sprintf(" in %d %s", len(repos), text.Plural(len(repos), "repo", "repos"))
	}
	line += fmt.Sprintf("  ·  %d safe, %d at risk, %d blocked", safe, atRisk, blocked)
	if hidden := len(res.Rows) - len(rows); hidden > 0 {
		line += fmt.Sprintf("  ·  %d hidden by --older-than", hidden)
	}
	return line
}

// parseAge parses Go durations plus whole days and weeks: 36h, 14d, 2w.
func parseAge(s string) (time.Duration, error) {
	for suffix, unit := range map[string]time.Duration{"d": 24 * time.Hour, "w": 7 * 24 * time.Hour} {
		if num, ok := strings.CutSuffix(s, suffix); ok {
			n, err := strconv.Atoi(num)
			if err != nil || n < 0 {
				return 0, fmt.Errorf("invalid duration %q", s)
			}
			return time.Duration(n) * unit, nil
		}
	}
	return time.ParseDuration(s)
}

func isTerminal(f *os.File) bool { return term.IsTerminal(f.Fd()) }

func usageErr(w io.Writer, format string, args ...any) int {
	fmt.Fprintf(w, "wtprune: "+format+" (run wtprune --help)\n", args...)
	return exitUsage
}
