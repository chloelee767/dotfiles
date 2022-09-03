# Dotfiles

Setup: `stow -R -t ~ -v <name of directory>` eg. `stow -R -t -v shell`

## Mac

Brew: https://brew.sh/

Hammerspoon: https://www.hammerspoon.org/

GNU coreutils:
``` sh
brew install coreutils ed findutils gawk gnu-sed gnu-tar grep make
```

Emacs
``` sh
brew tap railwaycat/emacsmacport
brew install --cask emacs-mac
```

Fonts:
``` sh
brew tap homebrew/cask-fonts
brew install --cask font-iosevka font-iosevka-nerd-font font-iosevka-ss14 font-iosevka-aile
brew install --cask font-noto-sans font-noto-sans-display font-noto-serif
```


Show only open applications in Dock:
```sh
defaults write com.apple.dock static-only -bool TRUE; killall Dock
```
