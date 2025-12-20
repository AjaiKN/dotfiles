# Ajai's dotfiles

## Downloading

To download my dotfiles, you can run either of the following commands:

<!-- sh -c "$(curl -fsSL https://dotfiles.ajai.dev/download)" -->
``` sh
curl https://dotfiles.ajai.dev/download | sh
```

<!-- sh -c "$(wget -qO - https://dotfiles.ajai.dev/download)" -->
``` sh
wget -O - https://dotfiles.ajai.dev/download | sh
```

By default, it will download to `~/prog/dotfiles`. If you want it to download to a different directory, set the `DOTFILES` environment variable to the location the repository should go.

Or you can just clone it yourself:

``` sh
git clone --depth=1 https://github.com/AjaiKN/dotfiles
```

If you don't have git installed, you should probably install it. But you can also download my dotfiles without git:

```sh
curl -LO 'https://github.com/AjaiKN/dotfiles/releases/latest/download/dotfiles.tar.gz'
# OR: wget 'https://github.com/AjaiKN/dotfiles/releases/latest/download/dotfiles.tar.gz'
tar xf dotfiles.tar.gz
```

## Installation

To install the dotfiles to your home directory, run `./install`.
(The download script will also offer to run this for you.)
If any of the relevant dotfiles already exist in the home directory, don't worry: the install script will prompt to ask you whether to skip that file, delete the original file, back up the original file, or [zap](https://github.com/AjaiKN/zap.sh) the original file to the trash.

## Uninstalling

If you want to remove all the symlinks to my dotfiles, run `./uninstall`.

NOTE: The `uninstall` script ONLY removes symlinks to my dotfiles. If you told the `install` script to delete, zap/trash, or back up any of the original files, the `uninstall` script will not undo that; you'll have to undo it yourself.

## Basic Repository Structure

- `config/`: Files that belong in `~/.config/` (`$XDG_CONFIG_HOME`)
- `dot-home/`: Files that go directly in my home directory (`~`)
- `bin/`: Custom scripts and utilities that belong in my PATH
- `scripts/`: Scripts that _don't_ need to be in my PATH
- `launchd/`: macOS LaunchAgents for background tasks
- `nix/`: Nix, NixOS, and Home Manager configurations
- `vendor/`: Third-party dependencies
- `private/`: A git submodule linking to a private repository containing configuration I haven't (yet) publicized

## Highlights

### Emacs

My very customized Doom Emacs configuration is in `config/doom/`.
See `config/doom/README.org`.

I have a bunch of custom Doom modules in `config/doom/modules/`.
See `config/doom/modules/README.org`.

### Shell configuration

- `config/shell/*.sh`: General shell configuration (for both bash and zsh)
- `config/zsh/`: Zsh configuration
  - `config/zsh/plugin-manager.zsh`: My custom zsh plugin manager
- `config/bash/`: Bash configuration

### Git
- `config/git/config`: Git configuration
- `config/gh/config.yml`: GitHub CLI configuration (including a bunch of aliases)
- Custom Git Subcommands (in `bin/`)
  - `git exclude`: Add to `.git/info/exclude`
  - `git ignore`: Add to `.gitignore`
  - `git force`: Force push relatively safely using `--force-with-lease` and `--force-if-includes`
  - `git ff`: Do a fast-forward merge
  - `git shelve`: Hide branches so they're not listed by `git branch` but are still accessible
    - `git unshelve`: Unhide a branch
    - `git shelved`: List hidden branches
  - `git stash-unstaged`: Stash only unstaged changes
  - `git undo-last-commit`: Safely undo the last commit while keeping changes
    - `git redo-commit`: Re-commit the undone commit (starting with the same commit message)

### Custom Scripts & Tools (in `bin/`)
- `zap`: Move files to the trash instead of deleting (supports both macOS and Linux, no dependencies except bash)
- `delete-ds-stores`: Clean up macOS .DS_Store files recursively
- `scripts/secure_path`: A best-effort attempt to make my PATH cleaner and more secure by removing potentially dangerous directories
- Emacs Integration
  - `magit`: Launch Magit (Emacs git interface) from command line
  - `calc`: Launch emacs [calc](https://www.gnu.org/software/emacs/manual/html_mono/calc.html) from the command line
  - `dired`: Browse a directory using [Dired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html)
  - `emacs-*`: Various ways of opening emacs

### Vim
- `dot-home/.vimrc`: Vim configuration (relatively minimal)
- `config/nvim/`: Neovim configuration

### Terminal
- `config/wezterm/`: [WezTerm](https://wezterm.org/index.html) terminal emulator config
- `dot-home/.tmux.conf`: tmux configuration

## License
Copyright (C) 2025 Ajai Khatri Nelson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
