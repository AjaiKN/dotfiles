#!/bin/sh

cd "$(dirname "$0")" || exit 1

# set -x

git submodule --quiet sync --recursive

# init all submodules except for private. (used ChatGPT)
# If you do want to init private, then run `git submodule init private`
for sm_path in $(git config --file .gitmodules --get-regexp path | awk '{print $2}'); do
	[ "$sm_path" = "private" ] || git submodule init "$sm_path"
done

# update all submodules
git -c submodule.fetchJobs=0 submodule update --depth=1 --recursive

:
