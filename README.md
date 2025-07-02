# Dotfiles

``` sh
git clone git@github.com:chloelee767/dotfiles.git
cd dotfiles
git submodule update --init
```

Sync changes to repo: `./sync-dir.sh <DIR>` eg. `./sync-dir.sh shell`

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
./sync-dir shell
./sync-dir general
./sync-dir doom
./sync-dir hammerspoon
./sync-dir work
./sync-dir kubernetes
```


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
