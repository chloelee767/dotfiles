# ghwatch

Watch GitHub Actions runs compactly: one line per job state change, then a verdict. Never fetches logs.

`gh run watch` reprints the whole job tree every few seconds (hundreds of KB for a long run). ghwatch prints roughly one line per job per state change, so a whole run fits in a few dozen lines -- useful for agents and background shells.

```
watching o/r branch main at a456cc9
run 36728797550  CI  https://github.com/o/r/actions/runs/36728797550
  0:00  in_progress  Lint, typecheck, test
  0:00  in_progress  cargo check
  1:27  success      Lint, typecheck, test
  8:10  success      cargo check
---
SUCCESS -- 2 job(s) passed
```

## Install

```bash
cd tools/ghwatch && go install .
```

Auth reuses `gh` (`gh auth login` or `GITHUB_TOKEN`).

## Usage

```bash
ghwatch                                   # current branch's tip commit, all workflows
ghwatch --branch feat/x --job 'gen-go'    # only jobs matching a regex
ghwatch --run https://github.com/o/r/actions/runs/123
ghwatch --pr 42 --workflow ci.yml
ghwatch --sha abc1234 --no-fail-fast --timeout 1h
```

Run `ghwatch --help` for every flag.

Behaviour worth knowing:

- **Branch/sha/PR targets watch every run for that commit**, re-listed on each poll. Runs that register late (right after a push) are picked up, and nothing from the previous commit is mistaken for the current one.
- **It stops when every run completes**, not when every *visible* job is done. Matrix jobs may not exist yet early in a run.
- **Fail fast by default**: it exits as soon as a matched job fails. `--no-fail-fast` waits for the rest.
- `--job` / `--exclude` scope both the output and the verdict.

Exit codes: `0` success, `1` failed / cancelled / no jobs ran, `2` timed out, `3` usage or lookup error.

## Development

```bash
go test ./...
```

`watch/` holds the polling loop and verdict logic behind a `Source` interface. Tests replay scripted snapshots through it with a fake clock. `github.go` is the only code that talks to GitHub.
