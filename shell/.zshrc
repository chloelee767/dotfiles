# useful guide: https://scriptingosx.com/2019/06/moving-to-zsh-part-3-shell-options/

# plugins
source ~/.zsh-plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.zsh-plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.zsh-plugins/zsh-z/zsh-z.plugin.zsh

setopt AUTO_CD # automatically cd to dirs
setopt NO_CASE_GLOB # case insensitive globbing
setopt AUTO_PUSHD # cd pushes to directory stack

# history
export HISTFILE="$HOME/.zsh_history"
setopt EXTENDED_HISTORY # save timestamp and execution time
SAVEHIST=10000 # how many lines to store in history file
HISTSIZE=4000 # how many lines to store in memory
setopt SHARE_HISTORY # share history across multiple zsh sessions
setopt APPEND_HISTORY # append to history, don't overwrite
# setopt INC_APPEND_HISTORY # add commands immediately after entering, rather than shell exit
setopt HIST_EXPIRE_DUPS_FIRST # expire duplicates first
# setopt HIST_IGNORE_DUPS # do not store duplicates
# setopt HIST_FIND_NO_DUPS # do not find duplicates when searching
setopt HIST_REDUCE_BLANKS # remove blank lines from history
setopt HIST_VERIFY # show substituted commmand

# correction
setopt CORRECT
setopt CORRECT_ALL

bindkey -e # use emacs keybindings, even if my default editor is vim
# Use bash-like word definitions for navigation and operations
autoload -U select-word-style
select-word-style bash

# Use C-u to kill to the beginning of the line
bindkey '^U' backward-kill-line

# Use C-x C-e to edit the current command line in $VISUAL (vim)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

## completions
zmodload -i zsh/complist
zstyle ':completion:*' menu select # menu driven completions
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==02=01}:${(s.:.)LS_COLORS}")' # colourful completions
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*' 'l:|=* r:|=*' # case insensitive matching
# organise completions: http://www.masterzen.fr/2009/04/19/in-love-with-zsh-part-one/
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format "$fg[yellow]%B--- %d%b"
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format "$fg[red]No matches for:$reset_color %d"
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

source ~/.shell_aliases

# fzf key bindings
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/chloe/google-cloud-sdk/path.zsh.inc' ]; then . '/home/chloe/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/chloe/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/chloe/google-cloud-sdk/completion.zsh.inc'; fi

# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh

eval "$(starship init zsh)"

autoload -U compinit && compinit # load completions
