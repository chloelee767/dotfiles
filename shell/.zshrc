# useful guide: https://scriptingosx.com/2019/06/moving-to-zsh-part-3-shell-options/
# zsh reference: https://zsh.sourceforge.io/Guide/zshguide02.html#l10

# keybinds
bindkey -e # use emacs keybindings, even if my default editor is vim
# Use C-x C-e to edit the current command line in $EDITOR (vim)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line
# C-u : redo
autoload -Uz redo
bindkey '^U' redo
# Don't consider special characters as part of a word
# default value: *?_-.[]~=/&;!#$%^(){}<>
WORDCHARS=""

setopt AUTO_CD # automatically cd to dirs
setopt AUTO_PUSHD # cd pushes to directory stack
setopt NO_CASE_GLOB # case insensitive globbing

# history
HISTFILE="$HOME/.zsh_history"
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
# setopt CORRECT_ALL

# completions
zmodload -i zsh/complist
zstyle ':completion:*' menu select # menu driven completions
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==02=01}:${(s.:.)LS_COLORS}")' # colourful completions
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*' 'l:|=* r:|=*' # case insensitive matching and partial matching
# organise completions: http://www.masterzen.fr/2009/04/19/in-love-with-zsh-part-one/
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format "$fg[yellow]%B--- %d%b"
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format "$fg[red]No matches for:$reset_color %d"
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# plugins
source ~/.zsh-plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.zsh-plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.zsh-plugins/zsh-z/zsh-z.plugin.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh # arch
[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ] && source /usr/share/doc/fzf/examples/key-bindings.zsh # ubuntu
[ -f /opt/homebrew/Cellar/fzf/0.47.0/shell/key-bindings.zsh ] && source /opt/homebrew/Cellar/fzf/0.47.0/shell/key-bindings.zsh

source ~/.shell_aliases

[ -f "$HOME/.zshrc.system" ] && source "$HOME/.zshrc.system"
[ -f "$HOME/.zshrc.local" ] && source "$HOME/.zshrc.local"

# function set_win_title(){
#     echo -ne "\033]0; $PWD \007"
# }
# precmd_functions+=(set_win_title)

eval "$(starship init zsh)"

# eval "$(direnv hook zsh)"

# load completions
autoload -U compinit
# only update zcompdump once a day since it's slow
# https://gist.github.com/ctechols/ca1035271ad134841284
if [ "$(find ~/.zcompdump -mtime +1)" ] ; then
    echo "Checking zsh completions cache..."
    compinit
fi
compinit -C
