#!/bin/sh

cd "${DOTFILES:=$(dirname "$0")}" || exit 1

# set -x

if ! command -v git >/dev/null 2>&1; then
	echo "git command not found; skipping submodule update" >&2
	exit 2
fi

if ! [ -e "$DOTFILES"/.git ]; then
	echo "$DOTFILES is not controlled by git; skipping submodule update" >&2
	exit 2
fi

git submodule --quiet sync --recursive

# init some submodules
# If you want to init private,     then run `./submodules-update.sh private`
# If you want to init emacs stuff, then run `./submodules-update.sh config/emacs config/doom`
git submodule init config/nano vendor config/zsh/themes "$@"

# ! command -v emacs >/dev/null 2>&1 || git submodule init config/emacs config/doom

# update all submodules
git -c submodule.fetchJobs=0 submodule update --init --recursive --depth=1 -- config/nano vendor config/zsh/themes "$@" || {
	echo "submodules-update.sh: shallow update failed (non-tip pin or an old git version?); retrying without --depth" >&2
	git -c submodule.fetchJobs=0 submodule update --init --recursive --no-recommend-shallow -- config/nano vendor config/zsh/themes "$@"
}

# shellcheck disable=SC2016
git submodule --quiet foreach '"$toplevel"/submodules-update--checkout-branch.sh'
