#!/bin/sh

# set -eu

cd "$(dirname "$0")" || exit 1

set -x

git submodule --quiet sync --recursive
# This should already be done, but this is in case I forgot to use
# --recurse-submodules when cloning.
git \
	-c submodule.recurse=true \
	-c submodule.fetchJobs=0 \
	submodule update --init --depth=1 --recursive
