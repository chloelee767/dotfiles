# (mostly) tested on Manjaro XFCE 20. But should work for other arch based distros.

##### Settings #####

# Whether to skip pacman/yay confirmations
# There are still other installers that have confirmations though (eg. doom)
YES="--noconfirm"
#YES=""

# Folder to save source code to for some programs
FOLDER="~/Documents/Code"
mkdir -p $FOLDER

##### Installation #####

## Install yay (also installs go)
cd $FOLDER && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si

## Remove some preinstalled applications
sudo pacman -R $YES thunderbird

## Terminal and shell
sudo pacman -S $YES kitty
sudo pacman -S $YES neovim ranger fzf
sudo pacman -S $YES unzip fd ripgrep cmake stow # note: fd and ripgrep are dependencies for doom

# zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" # say no to change shell, otherwise script will stop
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting
curl -fsSL https://starship.rs/install.sh | bash

## Other useful programs
yay -S $YES telegram-desktop

yay -S $YES emacs27-git
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d && ~/.emacs.d/bin/doom install # has quite a few prompts

## Programming
pacman -S $YES rust go-tools yarn texlive-most
cd $FOLDER && wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash # nvm

## Fonts
# TODO investigate font patching instead?
yay -S $YES ttf-ms-win10 ttf-iosevka ttf-roboto ttf-hack nerd-fonts-iosevka

## Finally, change shell
chsh -s /bin/zsh
