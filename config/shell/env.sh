# shellcheck shell=sh

# ~/.config/shell/env.sh
# Loaded by bash, zsh, and sh shells.
#   - zsh:  ~/.zshenv
#   - bash: $BASH_ENV, ~/.bash_profile, and ~/.bashrc
#   - sh:   $ENV, ~/.profile

## Load ~/.config/shell/envvars.sh if not already loaded

# shellcheck source=./envvars.sh
if [ -z "${AKN_SHELL_ENVVARS_LOADED:-}" ]; then
	if [ -n "${DOTFILES:-}" ]; then
		. "$DOTFILES/config/shell/envvars.sh" || :
	else
		. "$HOME/.config/shell/envvars.sh" || :
	fi
fi

if [ -n "${DOTFILES:-}" ] && [ -r "$DOTFILES/config/shell/env.sh" ]; then
	export ENV="$DOTFILES/config/shell/env.sh"
	export BASH_ENV="$ENV"
fi

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
