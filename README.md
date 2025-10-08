# Dotfiles

This repo contains my dotfiles for several machines.
The dotfiles are organised into multiple modules -- these are the top level folders such as shell, doom etc. 
On each machine, I opt-in to the subset of modules required.
Some modules are independent, some are mutually exclusive, some are dependent on each other.

The dotfiles for each module are "applied" to the machine using symlinks (using GNU stow).
The `./sync-dir.sh` script is used to "apply" modules, eg. `./sync-dir.sh shell`.
The script should be run after any files are added/removed to the dotfiles repo.

## Setup

```sh
git clone git@github.com:chloelee767/dotfiles.git
cd dotfiles
git submodule update --init
```

Install dependencies:
- fzf
- starship

Run `./sync-dir.sh` for the appropriate modules.

## Mac

Brew: https://brew.sh/

Hammerspoon: https://www.hammerspoon.org/

GNU coreutils:
``` sh
brew install coreutils ed findutils gawk gnu-sed gnu-tar grep make
```
They are added to the path in `work/.profile.system`

Emacs 30:
``` sh
brew tap d12frosted/emacs-plus
brew install emacs-plus@30 --with-native-comp
cp -r /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications
```

Fonts:
``` sh
brew tap homebrew/cask-fonts
brew install --cask font-iosevka font-iosevka-nerd-font font-iosevka-ss14 font-iosevka-aile
brew install --cask font-noto-sans font-noto-sans-display font-noto-serif
```

Setup:
```sh
./sync-dir shell general doom hammerspoon work kubernetes
```

## Overview of modules

TODO

## Archived

Install Emacs 28 on Mac:
``` sh
brew tap railwaycat/emacsmacport
brew install --cask emacs-mac
```

Show only open applications in Dock:
```sh
defaults write com.apple.dock static-only -bool TRUE; killall Dock
```
