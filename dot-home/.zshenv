# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout

## Nix prologue
OLD_PATH_ZSHENV=$PATH

## Stuff
source "$HOME/.shell_env.sh"


## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_ZSHENV:$PATH
fi
