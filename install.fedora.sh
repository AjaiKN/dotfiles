#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

set -x

sudo dnf -y update
sudo dnf -y upgrade
sudo dnf -y install git gh stow zsh curl wget make
sudo dnf -y install bsdtar fzf fd-find curl cmake editorconfig xclip tree ripgrep neovim shellcheck
sudo dnf -y install spice-vdagent qemu-guest-agent spice-webdavd

if ! gh auth token >/dev/null 2>&1; then
	gh auth login
fi

mkdir -p ~/prog
cd ~/prog
if ! [ -d ~/prog/dotfiles ]; then
	gh repo clone AjaiKN/dotfiles -- --depth=1
fi
cd dotfiles

./install-dot-home.sh

if [ "$(awk -F: -v user="$USER" '$1 == user {print $NF}' /etc/passwd)" != "$(command -v zsh)" ]; then
	chsh -s "$(command -v zsh)" "$USER" || echo "unable to change default shell to zsh"
fi
exec zsh
