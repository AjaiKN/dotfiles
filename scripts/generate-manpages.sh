#!/usr/bin/env bash

cd "$(dirname "$0")"/.. || exit 1

if ! command -v help2man >/dev/null 2>&1 || ! command -v ruby >/dev/null 2>&1; then
	if command -v nix-shell >/dev/null 2>&1; then
		exec nix-shell -p help2man ruby --run "$0"
	fi
fi

mkdir -p "${DOTFILES:-$HOME/prog/dotfiles}/man/man1" || exit 1

PATH="$(dirname "$0")/../bin:$PATH"
export PATH

commands=(trash volume git-ff delete-ds-stores concatpdf)
for c in "${commands[@]}"; do
	help2man --no-info --output="${DOTFILES:-$HOME/prog/dotfiles}/man/man1/$c.1" "$c"
done
