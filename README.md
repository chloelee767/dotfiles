# Dotfiles

This repo contains my dotfiles for several machines.
The dotfiles are organised into multiple modules -- these are the top level folders such as `shell`, `doom` etc.
On each machine, I opt-in to the subset of modules required.
Some modules are independent, some are mutually exclusive, some are dependent on each other.

The dotfiles for each module are enabled for each the machine using symlinks (using GNU stow).
The `./sync-dir.sh` script is used to enable modules, eg. `./sync-dir.sh shell`.
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

- An opionated zsh setup with settings, prompts, plugins, etc.
- Features include: auto-complete, syntax highlighting, shell command history, [z](https://github.com/rupa/z)
- Also includes setup for my commonly used programming languages.
- It tries to be reasonably performant while balancing maintainability and features.
- Designed to be compatible with both mac and different flavours of linux. OS/machine/use-case specific changes are in modules like `work`, `wsl` etc.
  - `.zshrc.system`/`.zshrc.local` and `.profile.system`/`.profile.local` files are used for this. Note that `.local` should be used for secrets and not commited to the repo.
- This module is practically always enabled, many other modules assume it will be present.

**Dependencies to install:**
- fzf
- starship

**Manual setup:**

Add this to the `~/.gitconfig` file:
```git-config
[include]
    path = ~/.gitconfig.common
```

### `general`

- Misc. shell scripts
- This module is practically always enabled, many other modules assume it will be present and will utilize the scripts defined here.

### Text Editors

#### `doom`

- Configs for [Doom Emacs](https://github.com/doomemacs/doomemacs), my current primary editor + IDE

#### `neovim`

- Configs for Neovim, my current lightweight editor

### `kubernetes`

- A collection of kubernetes scripts and configs for kubernetes-related tooling

### `hammerspoon`

- Configs for [Hammerspoon](https://www.hammerspoon.org/), a OSX automation and scripting library.
- Features: useful keyboard shortcuts

### Machines

These are modules with machine-specific configs, they are mutually exclusive.

#### `arch`

- Arch linux

#### `popos`

- Pop OS

**Setup notes:**

Ringboard:
```sh
systemctl --user enable ringboard-wayland
systemctl --user start ringboard-wayland
```
Note: we only need to enable & start ringboard-wayland. ringboard-server will get started by ringboard-wayland.

fcitx5:
- Open "language support" > "language", set "keyboard input method system" to fcitx5
- fcitx5 should already be installed, but as of PopOS 24.04, there are still a few missing packages: `sudo apt install zenity fcitx5-config-qt fcitx5-frontend-all`
- Now, the Fcitx 5 Configuration program will work 
- Add Fcitx5 as an autostart/startup program
- Useful docs:
  - https://fcitx-im.org/wiki/Setup_Fcitx_5
  - https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland

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

#### `wsl`

- WSL (ubuntu)
