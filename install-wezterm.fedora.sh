#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

sudo dnf copr enable wezfurlong/wezterm-nightly
sudo dnf install wezterm
