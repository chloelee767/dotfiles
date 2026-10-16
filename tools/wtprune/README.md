# wtprune

Prune old git worktrees interactively, and optionally delete their branches when nothing would be lost. Works on one repo, or on a whole folder of worktrees across repos (eg. `~/worktrees`).

You preview everything, edit the selection, review the plan, and confirm. At-risk worktrees go to the Trash instead of being deleted, and every action is logged.

```
wtprune  ~/worktrees  ·  189 worktrees in 52 repos  ·  3 selected (1 AT RISK)  ·  2 branches to delete (B: auto)

      REPO          WORKTREE             BRANCH              IDLE  STATUS            BRANCH ACTION
> [x] CatalogNCart  review-818           review-818           23d  clean             delete  pointer (= origin/feat-x)
  [x] Cats          listing-tier-msgs    chloe/listing-tier    4d  DIRTY 3M 1?       keep    2 unique
  [x] Bump-Service  listing-tier         tmp/lt-msg-check      2d  clean +1 ignored  delete  squash-merged (into origin/master)
  [ ] api-gateway   proto-bump           protobump-master      2d  clean             --      contained (in feat/x)
  [-] trust-chat    chatmod-x            chloe/x               1h  current
```

In `--dir` mode at 110+ columns the REPO column is shown and the `<repo>-` prefix is stripped from worktree names; narrower terminals drop REPO and show full names.

## Install

```bash
cd tools/wtprune && go install .
```

## Usage

```bash
wtprune                                    # linked worktrees of the repo you're in
wtprune --repo ~/go/src/github.com/o/r     # ... of another repo (any of its worktrees works)
wtprune --dir ~/worktrees --older-than 14d # every worktree under a folder, idle 2+ weeks
wtprune --dir ~/worktrees --select clean --delete-branches
wtprune --dir ~/worktrees --list           # print the table only; never changes anything
```

Run `wtprune --help` for every flag. `--list` is implied when stdin or stdout is not a terminal.

While scanning, a terminal stderr shows the phase: `finding worktrees under ~/worktrees...`, `loading repos 5/23...`, then `scanning 12/58...`, cut to the terminal width. Repos are loaded and worktrees inspected 8 at a time.

Exit codes: `0` ok (incl. quitting without changes), `1` some actions failed or were not attempted (interrupted), `2` usage or discovery error, `130` Ctrl-C during the scan.

## Keys

| Key | Action |
|---|---|
| `up`/`down`, `j`/`k`, `pgup`/`pgdn`, `g`/`G` | move |
| `space` | toggle the row and move to the next one |
| `a` | select all shown rows (incl. AT RISK -- they still need a typed `yes`) |
| `n` | select none, and clear every branch flag (all rows, also those hidden by the filter) |
| `c` | select only the SAFE rows shown |
| `b` | toggle deleting the row's branch, any deletable tier incl. `pushed`; also selects the row |
| `B` | auto branches on/off (see below) |
| `/` | filter by repo, worktree or branch; `enter` keeps it, `esc` clears it |
| `enter` | review the plan |
| `?` | help: keys and a legend of every status and tier |
| `q`, `ctrl+c` | quit without changing anything |

`a`, `c` and `B` only touch the rows the filter shows.

Branch flags only exist on selected rows: deselecting a row clears its flag. `B` (or `--delete-branches` at start) turns on **auto branches**, shown as `(B: auto)` in the header: every row that gets selected while it is on also gets its branch flagged if the branch is bulk-deletable (any tier but `pushed`), and turning it on flags the selected rows shown. Turning it off clears every flag. `b` flags or unflags one row by hand.

On the review screen with a typed-`yes` prompt, every printable key (incl. `j`, `k`, `q`) is typed; scroll with the arrows, `pgup`/`pgdn`, `home`/`end`.

## What the columns mean

IDLE: time since the last sign of use -- the newest of the HEAD commit date, the worktree's HEAD reflog (checkouts, commits, resets) and the mtimes of its changed files. `--` when nothing could be dated; `--older-than` never hides those rows.

STATUS:

- `clean` (green) -- **SAFE**: nothing beyond ignored build output would be lost.
- red, bold -- **AT RISK**: `DIRTY 2S 3M 1? 1U` (staged / modified / untracked entries / conflicted), `DETACHED +2 LOST` (commits no ref contains), `REBASING` and friends, `HIDDEN 3` (edits `git status` can't see: assume-unchanged, or skip-worktree files that differ from the index), `IN USE 3` (processes with their cwd inside; the detail pane lists them).
- `+N ignored` (yellow) -- ignored files such as `.env`, and ignored dirs that are not on the build-output allowlist (eg. `data/`). Not at risk on its own, but the worktree goes via the Trash. Allowlisted ignored dirs (`node_modules`, `build`, `dist`, `target`, `vendor`, `.venv`, `__pycache__`, `.next`, `coverage`, `DerivedData`, ... matched by basename) and `.DS_Store` are deleted without a Trash detour; the detail pane shows them dimmed.
- dimmed, checkbox `-` -- **blocked**, can't be selected: `current`, `locked`, `missing`, `moved`, `broken`, `submodules` (incl. embedded repos at a gitlink), `nested`, `error`. The detail pane says how to unblock it.

The detail pane also shows the upstream (`[gone]`, ahead/behind), the commits a detached HEAD would lose, and commits that only the worktree's reflogs still hold (a note, not a risk: rebases leave these all the time; their shas go into the log).

BRANCH ACTION: `delete` = flagged, `--` = deletable but not flagged, `keep` = not a candidate. A branch is a candidate only if nothing would be lost:

- `merged (in origin/master)` -- tip is in a default branch: a `refs/remotes/*/HEAD` target, or a local `main`/`master`/`develop`/`trunk`.
- `contained (in feat/x)` -- tip is in another local branch or a tag.
- `pointer (= origin/feat-x)` -- only in remote-tracking refs, and never committed to here (eg. a PR review branch).
- `squash-merged (into origin/master)` -- has unique commits, but their combined diff matches a commit on a default branch (`git patch-id`).
- `pushed, not merged` -- only in remote-tracking refs, but you committed to it. Auto branches skip it, so you need `b` on the row and a typed `yes`.
- `keep <reason>` -- `N unique`, `protected`, `in use elsewhere`, ... Detached rows show nothing.

## Safety

- **Review first**: `enter` shows the plan: at-risk worktrees with every file and LOST commit, safe ones, the branches to DELETE, the selected worktrees' branches it will KEEP and why, and anything dropped. Confirming takes `y`, or a typed `yes` + `enter` when anything at risk or a `pushed` branch is in the plan.
- **Batch check**: a branch whose commits are only in other branches of the same plan (eg. two branches at one unique commit) is kept: `only other ref is also being deleted (would lose N)`. Likewise a detached (or mid-operation) worktree whose HEAD is only in branches the plan deletes, eg. at a commit of a squash-merged branch, is left out: `holds N commits only squash-feat has (being deleted)`; unflag that branch (`b`) or remove the worktree on a later run. Execution re-checks both.
- **Re-scan before acting**: each worktree is re-inspected right before removal. If anything changed since the scan (HEAD, changed or ignored files, in-progress op, new process), it is skipped.
- **Parallel removal**: up to 4 worktrees are re-scanned and removed at once (`removing 12 worktrees and 11 branches, 4 at a time...`), each printing its own lines as it finishes. Branch checks and deletions still run one at a time per repo, so the batch check above still holds.
- **Trash**: at-risk worktrees and ones with ignored files or non-allowlisted ignored dirs are moved to `~/.Trash/wtprune-<name>-<time>` before deregistering. The trashed dir also gets `.wtprune-gitdir/`, a copy of the worktree's git admin dir (HEAD, reflog, index, rebase state). Its `.git` file is renamed to `.git.wtprune-orig`, so git commands run inside the trash dir fail instead of reaching a new worktree that git gave the same internal id. `--no-trash` force-removes them instead. Safe worktrees use plain `git worktree remove`, which refuses if they became dirty.
- **Branches** are deleted only after their worktree was removed, and only if a final check, reading the refs afresh, shows their commits are still in a ref that survives (squash-merged: the squash commit is still in its branch).
- **Log**: every action is appended as JSON to `~/.local/state/wtprune/log` (or `$XDG_STATE_HOME/wtprune/log`): path, HEAD sha, branch, changed and ignored paths, Trash location, LOST and reflog-only shas. A `trashing` line is written before the move, so a kill midway still leaves a record.
- **Ctrl-C** during execution prints `interrupted: finishing N in-flight worktrees; Ctrl-C again to abort`, finishes them, starts nothing new, then stops (exit 1; the rest count as not attempted). A second Ctrl-C kills it.

## Recovering

After a run that trashed anything, wtprune prints `restore: see "Recovering" ...` under the summary, which names the log.

1. Find the entry: `grep <worktree-name> ~/.local/state/wtprune/log | tail`. It has `head`, `branch`, `trash`, `lost` and `reflog_only`.
2. Get the commits back (works until `git gc` prunes them, ~2 weeks by default): `git -C <repo> branch rescue <sha>` for the `head`, a `lost` sha, or a deleted branch's `head`.
3. Recreate the worktree: `git -C <repo> worktree add <path> rescue`.
4. Copy files back from the trash dir (uncommitted changes, `.env`, ...) -- don't run git inside it or rename `.git.wtprune-orig` back: it points at an admin id git may have reused. Its `.wtprune-gitdir/` has the old index, HEAD reflog (`logs/HEAD`) and any in-progress rebase/merge state.

Testing knobs: `WTPRUNE_TRASH_DIR` and `WTPRUNE_LOG` override the Trash dir and the log path, so you can try it on throwaway repos without touching the real ones.

## Development

```bash
go test ./...
```

- `git/` runs git and parses its porcelain output.
- `scan/` discovers and inspects worktrees: risk flags, blocked states, branch tiers.
- `plan/` turns a selection into a plan and executes it.
- `tui/` is the bubbletea model. `Update` does no I/O, so tests drive it with key messages.
- `internal/text/` has display helpers shared by the CLI, TUI and plan output.

Tests run against real git repos in temp dirs; nothing is mocked.
