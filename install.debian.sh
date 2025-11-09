#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

cd "$(dirname "$0")"

# shellcheck disable=SC2016
: '

Getting started:

If on Ubuntu, search for Software & Updates settings, then make sure all the
boxes at the top are checked, especially the "universe" one
(https://askubuntu.com/a/148645).

# First, run:
sudo apt update

# Install git.
sudo apt install git

# Authenticate with git.
# The easiest way might be with gh (https://github.com/cli/cli/blob/trunk/docs/install_linux.md)
gh auth login

mkdir -p ~/prog && \
	git clone --depth=1 https://github.com/AjaiKN/dotfiles.git ~/prog/dotfiles && \
	cd ~/prog/dotfiles/ && \
	./install.linux.sh


'

./submodules-update.sh

sudo apt update
INSTALL="sudo apt-get install"
installcmd() {
	if ! hash "$1" 2>/dev/null; then
		$INSTALL "$1"
	fi
}

installcmd stow

./install-files.sh

# (not required)
installcmd curl
installcmd xclip
# INSTALL="sudo snap install --classic" installcmd nvim
if ! hash "nvim" 2>/dev/null; then
	installcmd neovim
fi
# recommended for doom emacs
if ! hash "fdfind" 2>/dev/null; then
	installcmd fd-find
	# https://github.com/sharkdp/fd?tab=readme-ov-file#on-ubuntu
	mkdir -p ~/.local/bin/
	ln -s "$(which fdfind)" ~/.local/bin/fd || :
fi
if hash "fdfind" 2>/dev/null && ! hash "fd" 2>/dev/null; then
	# https://github.com/sharkdp/fd?tab=readme-ov-file#on-ubuntu
	mkdir -p ~/.local/bin/
	ln -s "$(which fdfind)" ~/.local/bin/fd || :
fi
# for compiling emacs vterm
installcmd make
installcmd cmake
# for emacs editorconfig (not required, but it's super small, and the native
# elisp version is slower)
installcmd editorconfig

if command -v gsettings >/dev/null 2>&1; then
	# disable pressing command key to go to Activity Overview in GNOME
	# https://github.com/utmapp/UTM/issues/5411
	# https://askubuntu.com/a/1280308
	# https://askubuntu.com/a/1121073
	gsettings set org.gnome.mutter overlay-key '' || :
	gsettings set org.gnome.shell.keybindings toggle-overview "['<Super>s']" || :
	# gsettings reset org.gnome.mutter overlay-key
	# gsettings reset org.gnome.shell.keybindings toggle-overview
fi

# zsh (optional)
if ! hash zsh 2>/dev/null; then
	$INSTALL zsh
fi
chsh -s "$(command -v zsh)" "$USER" || echo "unable to change default shell to zsh"
exec zsh # MUST BE LAST THING IN THIS SCRIPT
