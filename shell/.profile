export EDITOR=/usr/bin/nvim
export TERMINAL=/usr/bin/kitty # doesn't work?
# location of setxkbmap layouts, options, etc.: /usr/share/X11/xkb/rules/base.lst
setxkbmap -option "ctrl:swapcaps" -option "altwin:swap_alt_win"

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
#export JAVA_HOME=/lib/jvm/java-13-jdk
#export PATH=$PATH:$JAVA_HOME/bin

# Go
#export GOPATH=$HOME/go
#export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/go/bin

##### Other shortcuts #####
# Laundro RPis
export PI_L9="pi@10.130.0.126"
export PI_L17="pi@10.130.0.84"

# cinnabot server
export cinnabot="root@188.166.247.136"
