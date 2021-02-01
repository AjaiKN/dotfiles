#!/usr/bin/env bash

# Usage: clear.sh

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 ": Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

cd "$(dirname "$0")"
set -x

nix profile remove --all
