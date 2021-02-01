#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

set -x

cd "$(dirname "$0")"
./submodules-update.sh

# # NOTE: the apt-get version is out of date.
# # Snap is already installed on Ubuntu.
# # On Debian, use `sudo apt update && sudo apt install snapd`.
# sudo snap install emacs --classic
# # sudo apt-get install emacs

EMACS_DIRNAME="$HOME/.local/build/emacs-$(mktemp tmp.XXXX)"
mkdir -p "$EMACS_DIRNAME"
cd "$EMACS_DIRNAME"
wget "https://gnu.mirror.constant.com/emacs/emacs-30.1.tar.xz"
tar xf emacs*
cd emacs*/
./configure --with-native-compilation --with-modules --with-gnutls --with-sqlite3
make
sudo make install

# Not using `apt` to install ripgrep because apparently the version there isn't
# built with PCRE support.
RIPGREP_VERSION="14.1.0"
RIPGREP_DIRNAME="$HOME/.local/build/ripgrep-$(mktemp tmp.XXXX)"
mkdir -p "$RIPGREP_DIRNAME"
cd "$RIPGREP_DIRNAME"
if [[ $(arch) == "aarch64" || $(arch) == "arm64" ]]; then
	RIPGREP_FILENAME=ripgrep-"$RIPGREP_VERSION"-aarch64-unknown-linux-gnu
	curl -LO https://github.com/BurntSushi/ripgrep/releases/download/"$RIPGREP_VERSION"/"$RIPGREP_FILENAME".tar.gz
	tar xvzf "$RIPGREP_FILENAME".tar.gz
	sudo cp "$RIPGREP_FILENAME"/rg /usr/local/bin/
else
	RIPGREP_FILENAME=ripgrep_"$RIPGREP_VERSION"_amd64.deb
	curl -LO https://github.com/BurntSushi/ripgrep/releases/download/"$RIPGREP_VERSION"/"$RIPGREP_FILENAME"
	sudo dpkg -i "$RIPGREP_FILENAME"
fi

set +x
echo "Optionally edit ~/.config/doom/local.el and add tags or module to akn/tags

$EDITOR ~/.config/doom/local.el

Then run:
doom install"
