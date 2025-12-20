#!/bin/sh

set -eu

if git symbolic-ref -q HEAD >/dev/null; then
	# HEAD not detached
	exit
fi

if branch=$(git config -f "$toplevel"/.gitmodules submodule."$name".branch); then
	head_commit=$(git rev-parse --abbrev-ref=strict HEAD)
	if ! git rev-parse --abbrev-ref=strict "origin/$branch" >/dev/null 2>&1; then
		git fetch --porcelain --update-shallow origin "$branch":"origin/$branch"
	fi
	if ! git rev-parse --abbrev-ref=strict "$branch" >/dev/null 2>&1; then
		git branch "$branch" "origin/$branch"
	fi
	if [ "$(git rev-parse --abbrev-ref=strict "origin/$branch")" = "$head_commit" ]; then
		"$DOTFILES"/bin/git-ff "$branch" "origin/$branch" 2>/dev/null ||
			echo "Can't fast-forward $branch to origin/$branch"
	fi
	if [ "$(git rev-parse --abbrev-ref=strict "$branch")" = "$head_commit" ]; then
		switch_to=$branch
	fi
fi

if [ "${switch_to:=$(git rev-parse --abbrev-ref=strict HEAD)}" != HEAD ]; then
	git checkout "${switch_to}" >/dev/null 2>&1
fi
