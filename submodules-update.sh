#!/bin/sh

cd "$(dirname "$0")" || exit 1

# set -x

if ! command -v git >/dev/null 2>&1; then
	echo "Error: git command not found; skipping submodule update"
fi

git submodule --quiet sync --recursive

# init all submodules except for private.
# If you do want to init private, then run `./submodules-update.sh private`
git submodule init config/nano vendor/fasd config/zsh/themes "$@"
# git submodule init private
# git submodule init config/emacs

# update all submodules
git -c submodule.fetchJobs=0 submodule update --depth=1 --recursive
