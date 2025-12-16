# shellcheck shell=bash

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
# shellcheck source=./colors.sh
. "$HOME/.config/shell/shared/colors.sh"

## Python: warn if venv isn't sourced
# shellcheck source=./python_venv.sh
. "$HOME/.config/shell/shared/python_venv.sh"

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
