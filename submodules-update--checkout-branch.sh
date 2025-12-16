#!/bin/sh

set -eu

if git symbolic-ref -q HEAD >/dev/null; then
	# HEAD not detached
	exit
fi

if branch=$(git config -f "$toplevel"/.gitmodules submodule."$name".branch); then
	head_commit=$(git rev-parse HEAD)
	if [ "$(git rev-parse "origin/$branch")" = "$head_commit" ]; then
		"$DOTFILES"/bin/git-ff "$branch" "origin/$branch" 2>/dev/null ||
			echo "Can't fast-forward $branch to origin/$branch"
	fi
	if [ "$(git rev-parse "$branch")" = "$head_commit" ]; then
		switch_to=$branch
	fi
fi

if [ "${switch_to:=$(git rev-parse --abbrev-ref=strict HEAD)}" != HEAD ]; then
	git checkout "${switch_to}" >/dev/null 2>&1
fi
