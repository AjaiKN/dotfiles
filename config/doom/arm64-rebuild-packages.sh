#!/usr/bin/env bash

# Run this script if when using tree-sitter, you get a warning about tsc-dyn.so

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

if ! command -v cargo && command -v nix-shell; then
	exec nix-shell -p cargo --run "$0"
fi

if cd "$DOTFILES/config/emacs/.local/straight/$(emacsclient --eval straight-build-dir | tr -d '"')/tsc"; then
	trash tsc-dyn.so || :
	cargo build --release
	ln -s ./target/release/libtsc_dyn.so tsc-dyn.so
fi
