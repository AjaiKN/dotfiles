# shellcheck shell=bash

# Loaded by interactive login shells in bash.
# On Mac, interactive shells are login shells by default.

# Find DOTFILES from this file's real location (see .bashrc)
__akn_bash_profile=${BASH_SOURCE[0]}
while [ -L "$__akn_bash_profile" ]; do
	__akn_link=$(readlink "$__akn_bash_profile")
	case $__akn_link in
		/*) __akn_bash_profile=$__akn_link ;;
		*) __akn_bash_profile=$(dirname "$__akn_bash_profile")/$__akn_link ;;
	esac
done
DOTFILES=$(cd -P "$(dirname "$__akn_bash_profile")/../.." && pwd -P) && export DOTFILES
unset __akn_bash_profile __akn_link

source "$DOTFILES/config/shell/profile.sh"

# We load .bashrc so that all interactive shells load .bashrc.
source "$DOTFILES/config/bash/.bashrc"

# https://docs.brew.sh/Shell-Completion
if type brew >/dev/null 2>&1
then
	HOMEBREW_PREFIX="$(brew --prefix)"
	# HOMEBREW_PREFIX="/opt/homebrew"
	if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]
	then
		source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
	else
		for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*
		do
			[[ -r "${COMPLETION}" ]] && source "${COMPLETION}"
		done
	fi
fi
