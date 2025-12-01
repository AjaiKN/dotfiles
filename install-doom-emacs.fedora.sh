#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

cd "$(dirname "$0")"
./install-files.sh

set -x

sudo dnf install emacs ripgrep fd-find editorconfig

set +x
echo "Optionally edit ~/.config/doom/local.el and add tags or module to akn/tags

$EDITOR ~/.config/doom/local.el

Then run:
doom install"
