#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
export PS4='$(printf "+%.0s" $(seq ${SHLVL:-1})) '
set -x

cd "$(dirname "$0")"

./submodules-update.sh

./install-defaults.mac.sh
./install-dot-home.sh
./install-set-sh-to-dash.mac.sh || :
