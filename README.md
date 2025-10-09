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

Run `./sync-dir.sh` for the appropriate modules.

## Overview of modules

### `shell`

- Base zsh environment.
- This module is practically always enabled, many other modules assume it will be present.
- It includes zsh settings, prompts, plugins, etc.
- Sets up various "APIs" for other modules to use, such as:
  - `.zshrc.system`, `.zshrc.local` system
- Designed to be compatible with both mac and different flavours of linux

**Dependencies to install:**
- fzf
- starship

### `general`

- Misc. shell scripts
- This module is practically always enabled, many other modules assume it will be present and will utilize the scripts defined here.

### Text Editors

#### `doom`

- Configs for [Doom Emacs](https://github.com/doomemacs/doomemacs), my current primary editor + IDE

#### `neovim`

- Configs for Neovim, my current fallback editor

### `kubernetes`

- A collection of kubernetes scripts and related tooling config

### `hammerspoon`

- Configs for [Hammerspoon](https://www.hammerspoon.org/), a OSX automation and scripting library.

### Desktop environments

These are modules with machine-specific configs, they are mutually exclusive.

#### `arch`

- Arch linux

#### `work`

- Work macbook

**Setup notes:**

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

