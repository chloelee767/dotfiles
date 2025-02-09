export EDITOR=/usr/bin/nvim
# export TERMINAL=/usr/bin/kitty # used by i3

# dir for ad-hoc binaries
PATH=$PATH:$HOME/bin/
PATH=$PATH:$HOME/.local/bin
# dir for ad-hoc zsh completions
fpath=($HOME/zsh-site-functions $fpath)

# ssh
# eval `ssh-agent`
# [ -f ~/.ssh/id_ed25519 ] && ssh-add ~/.ssh/id_ed25519
# [ -f ~/.ssh/id_rsa ] && ssh-add ~/.ssh/id_rsa

# shell tools
export FZF_DEFAULT_OPTS="--select-1"
# kubectx and kubens (personal fork)
export KUBECTX_FZF_USE_QUERY=1
export BAT_THEME="Coldark-Dark"
export K9S_CONFIG_DIR="$HOME/.config/k9s"
export GH_DASH_CONFIG="$HOME/.config/gh-dash/config.yml"

# emacs
PATH="$HOME/.emacs.d/bin:$PATH"

# Go
export GOPROXY="https://goproxy.carousellinternal.com,direct"
export GONOSUMDB="github.com/carousell/*"
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
PATH="$GOBIN:$PATH"
PATH="$HOME/.gotools:$PATH"
[[ -s "/Users/chloelee/.gvm/scripts/gvm" ]] && source "/Users/chloelee/.gvm/scripts/gvm"

# Rust
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# Javascript
export NVM_DIR="$HOME/.nvm"
if [ -d "$NVM_DIR" ]; then
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

# Other modules
[ -f "$HOME/.profile.system" ] && . "$HOME/.profile.system"
[ -f "$HOME/.profile.local" ] && . "$HOME/.profile.local"
