# Ajai's dotfiles

## Quick start

To download my dotfiles, you can run either of the following commands:

``` sh
sh -c "$(curl -fsSL https://dotfiles.ajai.dev/download)"
```

``` sh
sh -c "$(wget -qO - https://dotfiles.ajai.dev/download)"
```

By default, it will download to `~/prog/dotfiles`. If you want it to download to a different directory, set the `DOTFILES` environment variable to the location the repository should go.

Or you can just clone it yourself:

``` sh
git clone --depth=1 https://github.com/AjaiKN/dotfiles
```

## Installation

To install the dotfiles to your home directory, run `./install`.
(The download script will also offer to run this for you.)
If any of the relevant dotfiles already exist in the home directory, don't worry: the install script will prompt to ask you whether to skip that file, delete the original file, back up the original file, or move the original file to the trash.

## Uninstalling

If you want to remove all the symlinks to my dotfiles, run `./uninstall`.

NOTE: The `uninstall` script ONLY removes symlinks to my dotfiles. If you told the `install` script to delete, trash, or back up any of the original files, the `uninstall` script will not undo that; you'll have to undo it yourself.

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
