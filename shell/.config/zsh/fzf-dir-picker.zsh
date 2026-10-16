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
# The dir picker's query is a literal path. When it contains a "/" (or starts
# with "~") the candidate list gains the immediate children of the longest
# existing dir prefix, emitted in whatever form was typed, so the query stays a
# literal prefix and sorts to the top. tab replaces the query with the
# highlighted dir, similar to shell tab-completion but NOT a full match: the
# replaced query has no trailing "/", so it re-lists the PARENT's children
# (just the one dir you drilled into) until you type "/" yourself to descend.
# The static favourites + z list is always present too, so a z jump stays one
# fuzzy match away.
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
#                 alt+/      toggle dirs only (sticks across ctrl+t jumps)
#   dir picker:   ctrl+t     back to the file picker (in the last dir)
#                 tab        drill into the highlighted dir (replaces the query;
#                            type "/" after to actually descend, see below)
#                 alt+h      toggle showing hidden dirs
#                 alt+enter  accept the typed path verbatim (e.g. "../")

# File picker for $1 (a directory). Lists files + dirs, excluding .git.
# alt+h toggles hidden + gitignored and alt+/ toggles dirs-only, each via a
# state file + reload. ctrl+t jumps to the dir picker, remembering this dir so
# the dir picker's ctrl+t can come back here.
#
# The hidden state is per-invocation (resets after a ctrl+t jump); the dirs-only
# state lives in a file the widget owns, so it survives the whole become chain.
_fzf_file_from_dir() {
  local dir="${1:A}"                    # absolute (resolve ~, .., symlinks)
  export FZF_LAST_DIR="$dir"            # so the dir picker's ctrl+t can return here

  local statefile; statefile=$(mktemp)
  print -r -- 0 > "$statefile"          # 0 = default, 1 = show hidden + gitignored
  export FZF_FILE_STATE="$statefile"

  # Original dir (where ctrl+t launched)? Then insert relative + hide dir name.
  local accept_nth header
  local keys="ctrl+t: switch dir | alt+h: toggle hidden | alt+/: toggle dirs only"
  if [ "$dir" = "${FZF_ORIG_DIR:A}" ]; then
    accept_nth=2                        # output REL field
    header="$keys"
    export FZF_FILE_PROMPT_LABEL="Files"
  else
    accept_nth=1                        # output ABS field
    header="${dir}"$'\n'"$keys"
    export FZF_FILE_PROMPT_LABEL="Files in ${dir:t}"
  fi

  # The toggles end in change-prompt:... (colon form, must be last) rather than
  # change-prompt(...), so a ")" in a dir name can't cut the prompt short.
  FZF_DEFAULT_COMMAND="source \$FZF_FUNCS && _fzf_file_candidates" \
    fzf --multi --scheme=path --delimiter='\t' --with-nth=2 --accept-nth="$accept_nth" \
        --height "${FZF_TMUX_HEIGHT:-40%}" --min-height "20+" --layout=reverse \
        --prompt="$(_fzf_file_prompt)" \
        --header="$header" \
        --bind "ctrl-t:become(source \$FZF_FUNCS && _fzf_dir_switch)" \
        --bind "alt-h:transform:source \$FZF_FUNCS && _fzf_file_toggle \"\$FZF_FILE_STATE\"" \
        --bind "alt-/:transform:source \$FZF_FUNCS && _fzf_file_toggle \"\$FZF_DIRS_ONLY_STATE\""

  rm -f "$statefile"; unset FZF_FILE_STATE
}

_fzf_state_on() { [ -r "$1" ] && [ "$(cat "$1")" = 1 ]; }

# Candidate list for the file picker: "ABS<tab>REL" per line, honouring both
# toggles. Runs fd from $FZF_LAST_DIR so it emits relative paths; fzf shows REL
# and, on accept, outputs ABS when we've switched away from the original dir
# (valid from the shell cwd) or REL when we're still in it.
#
# --type l lists symlinks (neither f nor d to fd, so a dir of only symlinks --
# e.g. ~/templates -- would otherwise come up empty); --follow recurses into
# symlinked dirs and renders them with a trailing "/" like real ones. Keeping
# both means broken links still show rather than being silently dropped. In
# dirs-only mode --follow alone is enough: it classifies a symlink-to-dir as d.
_fzf_file_candidates() {
  local -a flags=(--follow --exclude .git)
  if _fzf_state_on "$FZF_DIRS_ONLY_STATE"; then
    flags+=(--type d)
  else
    flags+=(--type f --type d --type l)
  fi
  # --hidden reveals dotfiles; --no-ignore reveals .gitignore'd files
  _fzf_state_on "$FZF_FILE_STATE" && flags+=(--hidden --no-ignore)

  cd "$FZF_LAST_DIR" && fd "${flags[@]}" . \
    | awk '{print ENVIRON["FZF_LAST_DIR"] "/" $0 "\t" $0}'
}

_fzf_file_prompt() {
  local prompt="$FZF_FILE_PROMPT_LABEL"
  _fzf_state_on "$FZF_DIRS_ONLY_STATE" && prompt+=" [dirs]"
  _fzf_state_on "$FZF_FILE_STATE" && prompt+=" [+hidden]"
  print -r -- "${prompt}> "
}

# Flip a 0/1 state file and print the fzf actions to apply it (transform target).
_fzf_file_toggle() {
  if _fzf_state_on "$1"; then print -r -- 0 >| "$1"; else print -r -- 1 >| "$1"; fi
  print -r -- "reload(source \$FZF_FUNCS && _fzf_file_candidates)+change-prompt:$(_fzf_file_prompt)"
}

# Resolve a dir string (expand ~, make absolute) and, if it's a real dir,
# reopen the file picker there. Called from the dir picker's become bindings so
# the file picker's stdout flows straight up to the widget's capture (a shell
# $(...) around the dir picker's fzf would instead swallow the file selection).
_fzf_dir_resolve() {
  local dir="$1"
  [ -z "$dir" ] && return
  # ${~var} expands "~" only. It does NOT expand $VAR -- a "$HOME/x" query stays
  # literal and fails the -d test below, silently doing nothing. Deliberate:
  # expanding it would mean evaluating whatever was typed into the query field.
  dir=${~dir}
  dir=${dir:A}                         # resolve to absolute (handles ../, ., symlinks)
  [ -d "$dir" ] && _fzf_file_from_dir "$dir"
}

# Longest existing directory prefix of a query, in both the typed and resolved
# forms. Prints "TYPED_PREFIX<tab>RESOLVED_DIR" (typed always ends in /), or
# returns 1 if the query has no existing dir prefix.
#
# The typed form is preserved so candidates can be emitted the way the user
# typed them: a literal "~" never fuzzy-matches the expanded "/Users/...", so
# emitting absolute paths for a "~/..." query would tank the match score.
#
# Walks back a component at a time so a nonexistent MIDDLE component still
# yields a usable scan root (~/Code/nope/xyz -> ~/Code/). A single ${q%/*}
# would stop at ~/Code/nope/ and find nothing.
#
# Only "~" is expanded, never $VAR -- see the note in _fzf_dir_resolve. So a
# "$HOME/..." query gets no completion.
_fzf_dir_prefix_split() {
  local q="$1" typed prev resolved
  [ -z "$q" ] && return 1
  # Bare "~" has no slash but is still path-ish: treat it as "~/".
  [[ "$q" != */* ]] && q="${q}/"
  typed="${q%/*}/"
  while [ -n "$typed" ]; do
    resolved=${~typed}                 # expand "~" only, not $VAR
    resolved=${resolved:A}             # absolute; resolves .., ., symlinks
    if [ -d "$resolved" ]; then
      print -r -- "${typed}"$'\t'"${resolved}"
      return 0
    fi
    prev="$typed"
    typed="${typed%/}"                 # "a/b/" -> "a/b"
    typed="${typed%/*}"                # "a/b"  -> "a"
    typed="${typed:+${typed}/}"        # "a"    -> "a/"  (empty stays empty)
    # No progress means there is nothing left to walk back (e.g. "dotf/"),
    # so the query has no existing dir prefix.
    [ "$typed" = "$prev" ] && return 1
  done
  return 1
}

# Candidate list for the dir picker. Reads the live query from $FZF_QUERY (fzf
# exports it to child commands), so this works as both the initial list and the
# `change:reload` target.
#
# Always emits the static favourites + the z database. When the query looks like
# a path (contains a / or starts with ~) it ALSO emits the immediate children of
# the longest existing dir prefix, in the same form the user typed. Union rather
# than replacement: a better-ranked z hit stays reachable mid-drill.
#
# Hidden-dir toggle: reads $FZF_DIR_STATE (a per-invocation state file, mirroring
# the file picker's $FZF_FILE_STATE) to decide whether to pass --hidden to fd.
# Both the initial FZF_DEFAULT_COMMAND and the change:reload bind invoke this
# same function, so both automatically honour the toggle -- no separate
# hidden/non-hidden command string to keep in sync (unlike the file picker,
# which can afford two literal command strings because its command doesn't
# depend on a live query).
#
# Deliberately --hidden only, NOT --no-ignore (unlike the file picker's toggle):
# for a directory picker, gitignored dirs are dominated by things like
# node_modules and .git internals that you essentially never want to cd into,
# so surfacing them would add noise without adding reachable destinations.
# Dotfiles like .config are the actual daily papercut this fixes.
_fzf_dir_candidates() {
  local hidden_flag=""
  [ -r "$FZF_DIR_STATE" ] && [ "$(cat "$FZF_DIR_STATE")" = 1 ] && hidden_flag="--hidden"

  {
    print -l -- "$HOME" "$HOME/go/src/github.com/carousell/" /tmp
    # zsh-z database: path|rank|atime, sorted by rank desc
    local zdata=${ZSHZ_DATA:-${_Z_DATA:-$HOME/.z}}
    [ -r "$zdata" ] && sort -t'|' -k2 -rn "$zdata" | cut -d'|' -f1

    local q="$FZF_QUERY"
    if [[ "$q" == */* || "$q" == '~'* ]]; then
      local split typed resolved
      if split=$(_fzf_dir_prefix_split "$q"); then
        typed="${split%%$'\t'*}"
        resolved="${split##*$'\t'}"
        # Re-apply the typed prefix to each child's basename, so the query stays
        # a literal prefix of the candidate (see _fzf_dir_prefix_split).
        # --follow makes fd classify a symlink-to-dir as a dir, so symlinked
        # dirs are offered as destinations. --max-depth 1 caps the walk, so
        # there is no recursion (and no cycle risk) regardless.
        fd --type d --follow --max-depth 1 ${=hidden_flag} . "$resolved" 2>/dev/null \
          | while IFS= read -r p; do print -r -- "${typed}${p:t}"; done
      fi
    fi
  } | awk '!seen[$0]++'
}

# Directory picker. Enter opens the file picker in the highlighted dir;
# alt+enter forces the typed path (e.g. "../"); ctrl+t returns to the file
# picker in $FZF_LAST_DIR without choosing. Enter/alt+enter reopen via become so
# nothing here captures the file picker's output.
_fzf_dir_switch() {
  local statefile; statefile=$(mktemp)
  print -r -- 0 > "$statefile"          # 0 = default, 1 = show hidden dirs
  export FZF_DIR_STATE="$statefile"

  # --scheme=path is what makes drilling work: it scores a literal path prefix
  # far above a scattered fuzzy match, so the dir you are typing toward sorts
  # above unrelated z entries that happen to fuzzy-match.
  #
  # change:reload re-runs the generator on every keystroke. fd --max-depth 1 on
  # one dir is ~ms, so this is cheap; deliberately uncapped, since a cap would
  # silently hide directories.
  #
  # tab:replace-query swaps in the highlighted dir with no trailing "/", so the
  # next reload lists ITS parent's children (i.e. just the one dir you drilled
  # into) -- type "/" to actually descend, unlike real shell tab-completion.
  #
  # Keep every line below inside the backslash continuation: a "#" comment
  # between the continued lines would NOT be a comment, it would become an
  # argument to fzf and silently kill the binds that follow it.
  FZF_DEFAULT_COMMAND="source \$FZF_FUNCS && _fzf_dir_candidates" \
    fzf --scheme=path \
        --height "${FZF_TMUX_HEIGHT:-40%}" --min-height "20+" --layout=reverse \
        --prompt="Pick dir (or type a path)> " \
        --header="ctrl+t: back to files | tab: drill into dir (then / to descend) | alt+h: toggle hidden | alt+enter: use typed path" \
        --bind "change:reload(source \$FZF_FUNCS && _fzf_dir_candidates)" \
        --bind "tab:replace-query" \
        --bind "alt-h:transform:
          if [ \"\$(cat \$FZF_DIR_STATE)\" = 1 ]; then
            echo 0 > \$FZF_DIR_STATE
            echo 'change-prompt(Pick dir (or type a path)> )+reload(source \$FZF_FUNCS && _fzf_dir_candidates)'
          else
            echo 1 > \$FZF_DIR_STATE
            echo 'change-prompt(Pick dir (or type a path) [+hidden]> )+reload(source \$FZF_FUNCS && _fzf_dir_candidates)'
          fi
        " \
        --bind "enter:become(source \$FZF_FUNCS && _fzf_dir_resolve {})" \
        --bind "alt-enter:become(source \$FZF_FUNCS && _fzf_dir_resolve {q})" \
        --bind "ctrl-t:become(source \$FZF_FUNCS && _fzf_file_from_dir \"\$FZF_LAST_DIR\")"

  # NOTE: every exit bind above (enter/alt-enter/ctrl-t) is become(...), which
  # REPLACES this process, so this cleanup only runs on a plain Escape/Ctrl-C
  # exit -- the become paths leak the state file. mktemp defaults into
  # $TMPDIR, which the OS reaps, so this is an accepted tradeoff rather than a
  # real leak; restructuring every become target to also rm this file was
  # judged not worth the added coupling. Same shape as the file picker's
  # $FZF_FILE_STATE cleanup above, which has the identical limitation on its
  # one become bind (ctrl-t).
  rm -f "$statefile"; unset FZF_DIR_STATE
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

  # Dirs-only toggle state, owned here (not per picker) so it persists across
  # the become chain. Cleanup works because $(...) below waits for the chain.
  export FZF_DIRS_ONLY_STATE; FZF_DIRS_ONLY_STATE=$(mktemp)
  print -r -- 0 > "$FZF_DIRS_ONLY_STATE"

  # $(...) is a subshell that inherits this shell's functions, so no re-source
  # needed here (unlike the become subshells). fzf needs the real terminal.
  local selection item
  selection=$(_fzf_file_from_dir "$PWD" < /dev/tty)
  rm -f "$FZF_DIRS_ONLY_STATE"
  unset FZF_ORIG_DIR FZF_DIRS_ONLY_STATE

  # Insert each selected item, shell-quoted and space-separated, at the cursor.
  local out=""
  while IFS= read -r item; do
    [ -n "$item" ] && out+="${(q)item} "
  done <<< "$selection"
  LBUFFER="${LBUFFER}${out}"

  zle reset-prompt
}
zle -N _fzf_ctrl_t_widget
