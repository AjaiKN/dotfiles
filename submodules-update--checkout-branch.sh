#!/bin/sh

set -eu

if branch=$(git config -f "$toplevel"/.gitmodules submodule."$name".branch); then
	if git symbolic-ref -q HEAD >/dev/null; then
		# HEAD not detached
		exit
	fi
	if [ "$(git rev-parse HEAD)" = "$(git rev-parse "origin/$branch")" ]; then
		git ff "$branch" "origin/$branch" 2>/dev/null || {
			echo "Can't fast-forward $branch to origin/$branch"
			exit
		}
	fi
	if [ "$(git rev-parse HEAD)" = "$(git rev-parse "$branch")" ]; then
		git checkout "$branch" >/dev/null 2>&1
	# else
	# 	echo "Not switching $sm_path to a branch"
	fi
# else
# 	echo "No relevant branch for $sm_path"
fi
