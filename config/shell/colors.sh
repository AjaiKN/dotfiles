# shellcheck shell=bash

## Enabling colors

# if command -v dircolors >/dev/null 2>&1; then # proxy for GNU coreutils vs BSD
# 	test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
# 	alias ls='ls --color=auto'
#  	# alias dir='dir --color=auto'
# 	# alias vdir='vdir --color=auto'
# else
#   alias ls='ls -G'
# fi

# # copied from zsh4humans
# # LS_COLORS is used by GNU ls and Zsh completions. LSCOLORS is used by BSD ls.
# export LS_COLORS='fi=00:mi=00:mh=00:ln=01;36:or=01;31:di=01;34:ow=04;01;34:st=34:tw=04;34:'
# LS_COLORS+='pi=01;33:so=01;33:do=01;33:bd=01;33:cd=01;33:su=01;35:sg=01;35:ca=01;35:ex=01;32'
# export LSCOLORS='ExGxDxDxCxDxDxFxFxexEx'

### ls colors (from ohmyzsh)

# Default coloring for BSD-based ls:
export LSCOLORS="Gxfxcxdxbxegedabagacad"

# Default coloring for GNU-based ls:
# LS_COLORS equivalent to LSCOLORS (generated via https://geoff.greer.fm/lscolors)
export LS_COLORS="di=1;36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43"
# Define LS_COLORS via dircolors if available.
# if command -v dircolors >/dev/null 2>&1; then
# 	# shellcheck disable=SC2015 disable=1090
# 	[[ -f "$HOME/.dircolors" ]] &&
# 		source <(dircolors -b "$HOME/.dircolors") ||
# 		source <(dircolors -b)
# fi

function akn_try_ls_args {
	if command "$@" /dev/null &>/dev/null; then
		# shellcheck disable=SC2139
		if [ "$1" = ls ]; then
			alias ls="$*"
		else
			alias "$1"="$*"
			alias ls="$1"
		fi
		return 0
	else
		return 1
	fi
}

unalias ls 2>/dev/null
# Find the option for using colors in ls, depending on the version
case "$OSTYPE" in
	(netbsd*)
		# On NetBSD, test if `gls` (GNU ls) is installed (this one supports colors);
		# otherwise, leave ls as is, because NetBSD's ls doesn't support -G
		akn_try_ls_args gls --color=tty
		;;
	(openbsd*)
		# On OpenBSD, `gls` (ls from GNU coreutils) and `colorls` (ls from base,
		# with color and multibyte support) are available from ports.
		# `colorls` will be installed on purpose and can't be pulled in by installing
		# coreutils (which might be installed for other reasons), so prefer it to `gls`.
		akn_try_ls_args gls --color=tty
		akn_try_ls_args colorls -G
		;;
	(darwin*|freebsd*)
		# OMZ only uses GNU ls if installed and there are user defaults for
		# $LS_COLORS, as the default coloring scheme is not very pretty.
		# But I like that GNU ls is more compact.
		akn_try_ls_args gls --color=tty ||
			# This alias works by default just using $LSCOLORS
			akn_try_ls_args ls -G
		alias macls="\ls -G"
		# alias l="\ls -G -lAh -eOW" #-@
		alias getfacl="\ls -G -lde@"
		;;
	(*)
		akn_try_ls_args ls --color=tty ||
			akn_try_ls_args ls -G
		;;
esac
unset -f akn_try_ls_args

### diff colors

function diff {
	if command diff --color /dev/null /dev/null >/dev/null 2>&1; then
		command diff --color "$@"
	else
		command diff "$@"
	fi
}
