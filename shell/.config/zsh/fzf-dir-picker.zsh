# fzf file/directory picker with a dir-switch escape hatch.
#
# Flow: CTRL-T opens a file picker in $PWD. From there CTRL-T jumps to a
# directory picker; picking a dir (or typing a path) reopens the file picker in
# that dir. CTRL-T in the dir picker jumps back to the file picker in the last
# dir. The selected file path(s) get inserted at the cursor.
#
# Paths always display relative to the current picker dir (short lines). In the
# original dir ($PWD when ctrl+t launched) the relative path is inserted and the
# header omits the dir name; after switching elsewhere the absolute path is
# inserted and the header shows the dir. Switching back to the original dir
# restores the relative/no-header behaviour.
#
# The pickers chain via fzf's become(...), which fzf runs in a fresh `zsh -c`.
# That subshell sources only .zshenv -- NOT .zshrc -- so it lacks these
# functions. Each become command therefore `source $FZF_FUNCS` first ($FZF_FUNCS
# holds this file's path, exported below). Sourcing fresh per subshell is also
# what makes the recursive file<->dir chaining work.
#
# Keys:
#   file picker:  ctrl+t     switch to the directory picker
#                 alt+h      toggle showing hidden + gitignored files
#   dir picker:   ctrl+t     back to the file picker (in the last dir)
#                 alt+enter  accept the typed path verbatim (e.g. "../")

# File picker for $1 (a directory). Lists files + dirs, excluding .git.
# alt+h toggles hidden + gitignored via a per-invocation state file + reload.
# ctrl+t jumps to the dir picker, remembering this dir so the dir picker's
# ctrl+t can come back here.
_fzf_file_from_dir() {
  local dir="${1:A}"                    # absolute (resolve ~, .., symlinks)
  export FZF_LAST_DIR="$dir"            # so the dir picker's ctrl+t can return here

  local statefile; statefile=$(mktemp)
  print -r -- 0 > "$statefile"          # 0 = default, 1 = show hidden + gitignored
  export FZF_FILE_STATE="$statefile"

  # Paths shown relative to $dir (short list, even deep in the tree). Run fd from
  # $dir so it emits relative paths, then awk builds "ABS<tab>REL" per line
  # (prefixing $dir, read from ENVIRON so backslashes/spaces survive). fzf shows
  # the REL field and, on accept, outputs ABS when we've switched away from the
  # original dir (valid from the shell cwd) or REL when we're still in it.
  #
  # We deliberately avoid fd's --format {} placeholder: fzf treats {} in a
  # reload() command template as ITS OWN placeholder (the selected items), so
  # the two collide. Building the fields in awk keeps {} out of the command
  # entirely, so the same command works verbatim for the initial list and the
  # reload -- no fzf escaping needed.
  #
  # The awk program uses DOUBLE quotes (not single): the alt-h reload embeds this
  # command inside a single-quoted echo, and a single-quoted awk program would
  # collide with those. \$0 keeps $0 for awk (not the shell); FZF_LAST_DIR comes
  # from ENVIRON so no -v (which would mangle backslashes in the path).
  local fd_flags="--type f --type d --exclude .git"
  local awk_prog='awk "{print ENVIRON[\"FZF_LAST_DIR\"]\"/\"\$0\"\t\"\$0}"'
  local list_cmd="cd \"\$FZF_LAST_DIR\" && fd $fd_flags . | $awk_prog"
  # --hidden reveals dotfiles; --no-ignore reveals .gitignore'd files
  local list_cmd_hidden="cd \"\$FZF_LAST_DIR\" && fd $fd_flags --hidden --no-ignore . | $awk_prog"

  # Original dir (where ctrl+t launched)? Then insert relative + hide dir name.
  local accept_nth header prompt_label
  if [ "$dir" = "${FZF_ORIG_DIR:A}" ]; then
    accept_nth=2                        # output REL field
    header="ctrl+t: switch dir | alt+h: toggle hidden"
    prompt_label="Files"
  else
    accept_nth=1                        # output ABS field
    header="${dir}"$'\n'"ctrl+t: switch dir | alt+h: toggle hidden"
    prompt_label="Files in ${dir:t}"
  fi

  FZF_DEFAULT_COMMAND="$list_cmd" \
    fzf --multi --scheme=path --delimiter='\t' --with-nth=2 --accept-nth="$accept_nth" \
        --height "${FZF_TMUX_HEIGHT:-40%}" --min-height "20+" --layout=reverse \
        --prompt="${prompt_label}> " \
        --header="$header" \
        --bind "ctrl-t:become(source \$FZF_FUNCS && _fzf_dir_switch)" \
        --bind "alt-h:transform:
          if [ \"\$(cat \$FZF_FILE_STATE)\" = 1 ]; then
            echo 0 > \$FZF_FILE_STATE
            echo 'change-prompt(${prompt_label}> )+reload($list_cmd)'
          else
            echo 1 > \$FZF_FILE_STATE
            echo 'change-prompt(${prompt_label} [+hidden]> )+reload($list_cmd_hidden)'
          fi
        "

  rm -f "$statefile"; unset FZF_FILE_STATE
}

# Resolve a dir string (expand ~/vars, make absolute) and, if it's a real dir,
# reopen the file picker there. Called from the dir picker's become bindings so
# the file picker's stdout flows straight up to the widget's capture (a shell
# $(...) around the dir picker's fzf would instead swallow the file selection).
_fzf_dir_resolve() {
  local dir="$1"
  [ -z "$dir" ] && return
  dir=${~dir}                          # expand ~ and vars
  dir=${dir:A}                         # resolve to absolute (handles ../, ., symlinks)
  [ -d "$dir" ] && _fzf_file_from_dir "$dir"
}

# Directory picker. Enter opens the file picker in the highlighted dir;
# alt+enter forces the typed path (e.g. "../"); ctrl+t returns to the file
# picker in $FZF_LAST_DIR without choosing. Enter/alt+enter reopen via become so
# nothing here captures the file picker's output.
_fzf_dir_switch() {
  {
    print -l -- "$HOME" "$HOME/go/src/github.com/carousell/" /tmp
    # zsh-z database: path|rank|atime, sorted by rank desc
    local zdata=${ZSHZ_DATA:-${_Z_DATA:-$HOME/.z}}
    [ -r "$zdata" ] && sort -t'|' -k2 -rn "$zdata" | cut -d'|' -f1
    fd --type d . "$HOME" --max-depth 3 2>/dev/null
  } | awk '!seen[$0]++' \
    | fzf --height "${FZF_TMUX_HEIGHT:-40%}" --min-height "20+" --layout=reverse \
          --prompt="Pick dir (or type a path)> " \
          --header="ctrl+t: back to files | alt+enter: use typed path" \
          --bind "enter:become(source \$FZF_FUNCS && _fzf_dir_resolve {})" \
          --bind "alt-enter:become(source \$FZF_FUNCS && _fzf_dir_resolve {q})" \
          --bind "ctrl-t:become(source \$FZF_FUNCS && _fzf_file_from_dir \"\$FZF_LAST_DIR\")"
}

# Custom CTRL-T widget: replaces fzf's built-in fzf-file-widget. Opens the file
# picker in $PWD and inserts the shell-quoted selection at the cursor (like the
# built-in). The picker chain (become) prints the final selection to stdout,
# which we capture here. We own the widget so per-launch state lives here rather
# than in a static FZF_CTRL_T_OPTS string.
_fzf_ctrl_t_widget() {
  setopt localoptions pipefail no_aliases 2>/dev/null

  # Remember where ctrl+t started. The file picker shows paths relative to this
  # dir and inserts them relative while we're here; after switching elsewhere it
  # inserts absolute paths. Survives the whole become chain (exported).
  export FZF_ORIG_DIR="${PWD:A}"

  # $(...) is a subshell that inherits this shell's functions, so no re-source
  # needed here (unlike the become subshells). fzf needs the real terminal.
  local selection item
  selection=$(_fzf_file_from_dir "$PWD" < /dev/tty)
  unset FZF_ORIG_DIR

  # Insert each selected item, shell-quoted and space-separated, at the cursor.
  local out=""
  while IFS= read -r item; do
    [ -n "$item" ] && out+="${(q)item} "
  done <<< "$selection"
  LBUFFER="${LBUFFER}${out}"

  zle reset-prompt
}
zle -N _fzf_ctrl_t_widget
