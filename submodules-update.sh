#!/bin/sh

cd "$(dirname "$0")" || exit 1

# set -x

if ! command -v git >/dev/null 2>&1; then
	echo "Error: git command not found; skipping submodule update"
fi

git submodule --quiet sync --recursive

# init some submodules
# If you want to init private,     then run `./submodules-update.sh private`
# If you want to init emacs stuff, then run `./submodules-update.sh config/emacs config/doom`
git submodule init config/nano vendor/fasd config/zsh/themes "$@"

# update all submodules
git -c submodule.fetchJobs=0 submodule update --depth=1 --recursive
