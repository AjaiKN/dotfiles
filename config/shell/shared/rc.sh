# shellcheck shell=bash
# shellcheck disable=SC2317

# NOTE: This file loaded in both .bashrc (which is loaded by .bash_profile) and .zshrc.
# So it's loaded in all interactive shells in both bash and zsh.

echo_if_interactive() {
	if [ -t 0 ]; then
		echo "$@"
	fi
}

# echo_if_interactive "Loading ~/.config/shell/shared/rc.sh"

## Nix Prologue
OLD_PATH_SHELL_RC=$PATH

## Load envvars (in case they've changed)
# shellcheck source=./envvars.sh
. "$HOME/.config/shell/shared/envvars.sh" || :

## Rosetta 2
# Open Rosetta 2 version of zsh
alias i="arch -x86_64 /bin/zsh"

## Functions and Aliases
# shellcheck source=./aliases_and_functions.sh
. "$HOME/.config/shell/shared/aliases_and_functions.sh"

## Enable colors

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

## Python: warn if venv isn't sourced

venv-find() {
	local dir
	dir=${1:-$PWD} || return 2
	while [[ $dir != / ]] && [[ $dir != "$HOME" ]]; do
		[[ -d "$dir/venv" ]] && printf '%s' "$dir/venv" && return
		[[ -d "$dir/.venv" ]] && printf '%s' "$dir/.venv" && return
		dir=$(dirname "$dir")
	done
	return 1
}

venv-activate() {
	local venv
	venv=$(venv-find) || return 2
	echo source "$venv/bin/activate"
	# shellcheck disable=SC1091
	source "$venv/bin/activate"
}

venv-check() {
	local COLOR='\033[33m'
	local NOCOLOR='\033[0m'
	if [[ -z "${VIRTUAL_ENV:-}" ]]; then
		local venv
		venv=$(venv-find) || return
		echo -e "${COLOR}Warning: venv is not sourced: source $venv/bin/activate${NOCOLOR}"
	fi
}

venv-direnv() {
	local venv
	venv=$(venv-find) || return 2
	touch .envrc
	printf 'export VIRTUAL_ENV=.venv
layout python3
' >>.envrc
	cat .envrc
}

alias activate="venv-activate"
alias venv-source="venv-activate"

alias venv-make="python -m venv .venv"
alias venv-create="python -m venv .venv"
alias mkvenv="python -m venv .venv"

alias venv-uv="uv venv"
alias venv-make-with-uv="uv venv"
alias venv-create-with-uv="uv venv"
alias mkvenv-uv="uv venv"

if [[ $TERM != dumb ]]; then
	if [[ -n "${BASH_VERSINFO[0]:-}" ]]; then
		if [[ $PROMPT_COMMAND != *venv-check* ]]; then
			PROMPT_COMMAND="${PROMPT_COMMAND:-':'};venv-check"
		fi
	elif [[ -n "${ZSH_VERSION:-}" ]]; then
		# TODO: improve perf; adds command lag of ~8ms (zsh-bench)
		autoload -Uz add-zsh-hook
		add-zsh-hook precmd venv-check
	fi
fi

## Mise
# https://mise.jdx.dev/lang/ruby.html#configuration
# ruby-build was being super slow, so use ruby-install instead
# export MISE_RUBY_INSTALL=true

## Pagers

# syntax highlighting for less using GNU source-highlight
# https://superuser.com/a/71593
lesspipe=$(command -v src-hilite-lesspipe.sh 2>/dev/null || command -v lesspipe.sh 2>/dev/null)
[ -z "$lesspipe" ] && [ -e /usr/share/source-highlight/src-hilite-lesspipe.sh ] && lesspipe=/usr/share/source-highlight/src-hilite-lesspipe.sh
[ -z "$lesspipe" ] && [ -e /usr/bin/source-highlight/src-hilite-lesspipe.sh ] && lesspipe=/usr/bin/source-highlight/src-hilite-lesspipe.sh
if [ -z "$LESSOPEN" ] && [ -n "$lesspipe" ]; then
	export LESSOPEN="| $lesspipe %s"
	export LESSCLOSE=""
fi
unset lesspipe

## GPG
# if $TTY is defined and not empty
if [ -n "$TTY" ]; then
	# faster but only works in zsh
	# https://github.com/romkatv/powerlevel10k/blob/master/README.md#how-do-i-export-gpg_tty-when-using-instant-prompt
	GPG_TTY=$TTY
else
	GPG_TTY=$(tty)
fi
export GPG_TTY

## fasd
# Override fasd builtin interactive aliases to use fzf
# https://github.com/clvv/fasd#introduction
unalias sf sd zz 2>/dev/null
sf () {
	# interactive file selection
	fasd -f -l | fzf --tac --tiebreak=index --query="$*"
}
sd () {
	# interactive directory selection
	fasd -d -l | fzf --tac --tiebreak=index --query="$*"
}
zz () {
	# cd with interactive selection
	cd "$(fasd -d -l | fzf --tac --tiebreak=index --query="$*")" || return 1
}
alias e='f -e "$EDITOR"'

## bad package.json files
# check if there are any package.json files that shouldn't be there
(if [ -f /package.json ] || [ -f /Users/package.json ] || [ -f ~/package.json ] || [ -f ~/prog/package.json ]; then
	>&2 echo_if_interactive "Warning: package.json found in a location where it probably shouldn't be"
fi &)

## thefuck
# output of `thefuck --alias`
# could also use the ohmyzsh plugin
fuck () {
	TF_PYTHONIOENCODING=$PYTHONIOENCODING;
	export TF_SHELL=zsh;
	export TF_ALIAS=fuck;
	TF_SHELL_ALIASES=$(alias);
	export TF_SHELL_ALIASES;
	TF_HISTORY="$(fc -ln -10)";
	export TF_HISTORY;
	export PYTHONIOENCODING=utf-8;
	TF_CMD=$(
		thefuck THEFUCK_ARGUMENT_PLACEHOLDER $@
				) && eval $TF_CMD;
	unset TF_HISTORY;
	export PYTHONIOENCODING=$TF_PYTHONIOENCODING;
	test -n "$TF_CMD" && print -s $TF_CMD
}

## wezterm integration
if [ -n "$WEZTERM_EXECUTABLE_DIR" ]; then
	WEZTERM_SHELL_INTEGRATION="$WEZTERM_EXECUTABLE_DIR"/../Resources/wezterm.sh
	if [ -f "$WEZTERM_SHELL_INTEGRATION" ]; then
		# shellcheck source=/Applications/WezTerm.app/Contents/Resources/wezterm.sh
		source "$WEZTERM_SHELL_INTEGRATION"
	fi
fi

## Nix Epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	echo_if_interactive "We're in Nix!"
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_SHELL_RC:$PATH
fi

# echo_if_interactive "Loaded ~/.config/shell/shared/rc.sh"
