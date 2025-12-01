# shellcheck shell=bash

# Loaded by interactive login shells in bash.
# On Mac, interactive shells are login shells by default.

source "$HOME/.config/shell/shared/profile.sh"

# We load .bashrc so that all interactive shells load .bashrc.
source "$HOME/.bashrc"

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
