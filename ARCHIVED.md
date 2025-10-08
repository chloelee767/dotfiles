# Archived from README.md

Install Emacs 28 on Mac:
``` sh
brew tap railwaycat/emacsmacport
brew install --cask emacs-mac
```

Show only open applications in Dock:
```sh
defaults write com.apple.dock static-only -bool TRUE; killall Dock
```
