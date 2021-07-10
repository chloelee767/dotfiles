export EDITOR=/usr/bin/nvim
export TERMINAL=/usr/bin/kitty # used by i3

export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/dotfiles/scripts # scripts
export PATH=$PATH:$HOME/.emacs.d/bin # doom emacs

# programming languages

export PATH="$HOME/.cargo/bin:$PATH"

#export JAVA_HOME=/usr/lib/jvm/java-14-openjdk
#export PATH=$PATH:$JAVA_HOME/bin
#export PATH=$PATH:$HOME/.jenv/bin

#export GOPATH=$HOME/go
#export PATH=$PATH:$GOPATH/bin
# export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/go/bin
export GO111MODULE=auto

# export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

[ -f "$HOME/.profile.system" ] && . "$HOME/.profile.system"
[ -f "$HOME/.profile.local" ] && . "$HOME/.profile.local"
