export EDITOR=/usr/bin/nvim

## ALIASES
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

alias vim="nvim"

alias py3="python3"
alias py="python"

alias gs="git status"
alias gsta="git stash"
alias ga="git add"
alias gc="git commit"
alias gco="git checkout"
alias gpl="git pull"
alias gph="git push"
alias gd="git diff"
alias gb="git branch"

## PATHS
export PATH=$PATH:$HOME/bin

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# keyboard: swap caps and escape
setxkbmap -option "caps:swapescape"

# emacs
export PATH=$PATH:$HOME/.emacs.d/bin

# Java
export JAVA_HOME=/lib/jvm/java-13-jdk
export PATH=$PATH:$JAVA_HOME/bin

# Go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/go/bin

# nvm
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
