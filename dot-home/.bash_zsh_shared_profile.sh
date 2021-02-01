# shellcheck shell=bash
# shellcheck disable=SC2317

## Nix prologue
OLD_PATH_BASH_ZSH_SHARED_PROFILE=$PATH

## Stuff
# Loaded by both ~/.bash_profile and ~/.zprofile.

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_BASH_ZSH_SHARED_PROFILE:$PATH
fi
