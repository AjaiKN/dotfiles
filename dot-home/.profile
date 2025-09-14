## Nix prologue
OLD_PATH_PROFILE=$PATH

## Stuff

. "$HOME/.shell_profile.sh"

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_PROFILE:$PATH
fi
