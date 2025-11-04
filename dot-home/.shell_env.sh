# shellcheck shell=sh

# .shell_env.sh
# Loaded by bash, zsh, and sh shells.
#   - ~/.zshenv (zsh)
#   - ~/.bash_profile and ~/.bashrc (bash)
#   - ~/.profile (sh and others)

export ENV="$HOME/.shell_env.sh"
export BASH_ENV="$HOME/.shell_env.sh"

## Nix prologue
OLD_PATH_SHELL_ENV=$PATH

## umask
umask o-w || :

## Load .shell_envvars.sh if not already loaded

[ -n "${AKN_SHELL_ENVVARS_LOADED:-}" ] || . "$HOME/.shell_envvars.sh" || :

## PS4 (used by set -x and bash -x)
# $PS4 is the prefix used by `set -x`, and it's '+ ' by default.
# $SHLVL is the current number of nested shells.
#
# If I have a bash script that uses `set -x`, and that calls another bash script
# which uses `set -x`, I want to make it clear that the commands are nested a
# level further. To do this, I insert an extra $SHLVL "+"s into $PS4.
#
# In POSIX sh, PS4 only has to support parameter expansion (env vars ${}), not
# command substitution. For example, dash doesn't support command subsitution
# in PS4. So we check that we're in either bash or zsh.
if [ -n "$BASH_VERSION" ]; then #|| [ -n "$ZSH_VERSION" ]; then
	PS4='$(printf "+%.0s" $(seq ${SHLVL:-1})) '
	# Don't export so it doesn't get inherited by shells like dash.
fi

## Secure PATH

if [ -x "$DOTFILES/scripts/secure_path" ]; then
	if [ -t 0 ]; then
		PATH="$("$DOTFILES/scripts/secure_path" || printf '%s' "$PATH")"
	else
		# if not interactive, don't let it print anything
		PATH="$("$DOTFILES/scripts/secure_path" 2>/dev/null || printf '%s' "$PATH")"
	fi
	export PATH
fi

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_SHELL_ENV:$PATH
fi
