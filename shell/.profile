export EDITOR=/usr/bin/nvim
export TERMINAL=/usr/bin/kitty # doesn't work?
# location of setxkbmap layouts, options, etc.: /usr/share/X11/xkb/rules/base.lst
setxkbmap -option "ctrl:swapcaps_hyper,altwin:swap_lalt_lwin"

##### Paths #####

# scripts
export PATH=$PATH:$HOME/dotfiles/scripts

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# LSM3241 bcftools
# export BCFTOOLS_PLUGINS=$HOME/Documents/Code/bcftools/plugins

# emacs
export PATH=$PATH:$HOME/.emacs.d/bin

# Java
#export JAVA_HOME=/usr/lib/jvm/java-14-openjdk
#export PATH=$PATH:$JAVA_HOME/bin
export PATH=$PATH:$HOME/.jenv/bin

# Go
#export GOPATH=$HOME/go
#export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/go/bin
export GO111MODULE=auto

# nvm
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

##### Other shortcuts #####
# Laundro RPis
export PI_L9="pi@10.130.0.126"
export PI_L17="pi@10.130.0.84"

# cinnabot server
export cinnabot="root@188.166.247.136"
