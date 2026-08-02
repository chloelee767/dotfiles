# AGENTS.md

## Repo Layout

- This repo is a dotfiles repo organized as top-level modules such as `shell/`, `general/`, `neovim/`, `work/`.
- Most top-level module folders are GNU Stow packages. Paths inside a module are home-relative:
  - `shell/.zshrc` -> `~/.zshrc`
  - `neovim/.config/nvim/init.vim` -> `~/.config/nvim/init.vim`
  - `hammerspoon/.hammerspoon/init.lua` -> `~/.hammerspoon/init.lua`
- `tests/` is intentionally outside the stow modules so tests do not get symlinked into `$HOME`.
- For module-specific setup notes, machine-specific details, and higher-level background, read `README.md`.

## How Syncing Works

- Use `./sync-dir.sh <module>...` to apply one or more modules.
- `sync-dir.sh` does 3 things:
  - runs repo-level `./setup.sh` once if present
  - runs `<module>/setup.sh` before syncing that module, if present
  - runs `stow -R -t ~` for each module
- `stow` ignores `setup.sh` and `README.md`, so those files are not linked into `$HOME`.
- When you add, remove, or move files inside a module, re-run `./sync-dir.sh` for the affected modules so the symlinks match the repo.

## Driving fzf Widgets Headlessly

- You can use the `headless-terminal` skill (`ht`) to test interactive shell widgets (eg. the CTRL-T picker in `shell/.config/zsh/fzf-dir-picker.zsh`).
- **Gotcha:** fzf with `--height` below 100% renders a blank, unresponsive screen under `ht`. Partial-height mode draws inline into a scroll region that `ht`'s VT engine does not render -- the process stays alive but never paints or accepts keys.
  - Repro: `printf 'a\nb\n' | fzf --height 40%` is blank; `--height 100%` works.
  - Fix: export `FZF_TMUX_HEIGHT=100%` in the `ht` session before triggering the widget. The picker functions already read that var, so no code changes are needed.
- Waits: `become(...)` re-execs fzf, so use `--wait-text` on the next prompt (eg. `Files>`, `Pick dir`) rather than `--wait-idle`, and allow a generous `--timeout` since the dir picker scans `$HOME`.
- A `become`-chained picker can orphan itself if its terminal dies -- check for stale `fzf --height` processes (`ps -ax | grep '[f]zf --height'`) if things behave oddly.

## Tests

- Tests live under top-level `tests/`.
- Current layout is grouped by module, then language/tooling:
  - `tests/work/`
  - `tests/work/python/`
- Current commands:
  - `just test` runs all tests via `python3 -m unittest discover tests`
  - `just test-work-python` runs `python3 -m unittest discover tests/work/python`
  - read the justfile to get the complete and up to date list of commands
