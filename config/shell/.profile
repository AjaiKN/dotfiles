# shellcheck shell=sh

## Nix prologue
OLD_PATH_PROFILE=$PATH

## Stuff

# shellcheck source=./profile.sh
. "$HOME/.config/shell/profile.sh"

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_PROFILE:$PATH
fi
