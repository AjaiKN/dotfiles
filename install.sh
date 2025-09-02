#!/bin/sh

set -eu

DOTFILES=$(realpath "$(dirname "$0")")
export DOTFILES
cd "$DOTFILES"

if [ "$(uname -s)" = "Darwin" ]; then
	./install-defaults.mac.sh
fi

./install-dot-home.sh
