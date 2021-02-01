#!/usr/bin/env bash

# Usage: install.sh packages...

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 ": Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

cd "$(dirname "$0")"
set -x

for arg in "$@"; do
	nix profile install --impure --expr "import ./$arg.nix (import <nixpkgs> {})"
done
