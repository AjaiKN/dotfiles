#!/usr/bin/env zsh

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

# https://github.com/umlx5h/zsh-manpage-completion-generator
# brew install umlx5h/tap/zsh-manpage-completion-generator

fish -c 'fish_update_completions'
zsh-manpage-completion-generator
