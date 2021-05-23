# Dotfiles

Setup: `stow -R -t ~ -v <name of directory>` eg. `stow -R -t -v shell`

## Mac

Install homebrew:

``` sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Install emacs:

``` sh
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
```

Install fonts:

``` sh
brew tap homebrew/cask-fonts
brew install --cask font-iosevka font-iosevka-nerd-font
brew install --cask font-noto-sans font-noto-sans-display font-noto-serif
```

Show only open applications in Dock:

```sh
defaults write com.apple.dock static-only -bool TRUE; killall Dock
```
