# Ajai's dotfiles

## Quick start

To download my dotfiles, you can run either of the following commands:

``` sh
sh -c "$(curl -fL https://dotfiles.ajai.dev/download.sh)"
```

``` sh
sh -c "$(wget -O - https://dotfiles.ajai.dev/download.sh)"
```

By default, it will download to `~/prog/dotfiles`. If you want it to download to a different directory, set the `DOTFILES` environment variable to the location the repository should go.

## Installation

To install the dotfiles to your home directory, run `./install.sh`.
(The download script will also offer to run this for you.)
If any of the relevant dotfiles already exist in the home directory, don't worry: the install script will prompt to ask you whether to skip that file, delete the original file, back up the original file, or move the original file to the trash.

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
