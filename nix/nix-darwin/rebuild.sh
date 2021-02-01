#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 ": Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

cd "$(dirname "$0")"

exec sudo ./darwin-rebuild switch --flake . -I darwin-config="$(realpath ./configuration.nix)"
