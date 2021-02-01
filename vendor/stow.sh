#!/usr/bin/env bash

cd "$(dirname "$0")" || exit 1

# shellcheck disable=SC2035
exec stow -v --restow *.plugin
