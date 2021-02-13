export EDITOR=/usr/bin/nvim
export TERMINAL=/usr/bin/kitty # doesn't work?
# location of setxkbmap layouts, options, etc.: /usr/share/X11/xkb/rules/base.lst
setxkbmap -option "ctrl:swapcaps_hyper,altwin:swap_lalt_lwin"

##### Paths #####

export PATH=$PATH:$HOME/bin

# scripts
export PATH=$PATH:$HOME/dotfiles/scripts

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

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

##### Projects #####

export SUNFIRE=e0325190@sunfire.comp.nus.edu.sg

export NSCC=e0325190@nus.nscc.sg
export NSCC_DIR=e0325190@nus.nscc.sg:/home/users/nus/e0325190
# gurobi license file
export GRB_LICENSE_FILE=/home/chloe/programs/gurobi811/my_license/gurobi.lic

[[ -f ~/.private_profile ]] && ~/.private_profile

# LSM3241 bcftools
# export BCFTOOLS_PLUGINS=$HOME/Documents/Code/bcftools/plugins

# export PATH=$PATH:$HOME/Code/monocle-scripts/exec/
