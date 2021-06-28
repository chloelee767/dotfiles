export EDITOR=/usr/bin/nvim
export TERMINAL=/usr/bin/kitty # doesn't work?

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

# Gurobi
export GRB_LICENSE_FILE=$HOME/programs/gurobi811/my_license/gurobi.lic
export GUROBI_HOME=$HOME/programs/gurobi811/linux64/
export PATH=${PATH}:${GUROBI_HOME}/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:~/programs/gurobi811/linux64/lib
# /etc/ld.so.conf.d/gurobi.conf was also added, although it doesn't seem to have any effect

# comets
export COMETS_HOME=$HOME/programs/comets/


swap-win-alt.sh

[[ -f ~/.private_profile ]] && ~/.private_profile

# LSM3241 bcftools
# export BCFTOOLS_PLUGINS=$HOME/Documents/Code/bcftools/plugins

# export PATH=$PATH:$HOME/Code/monocle-scripts/exec/
