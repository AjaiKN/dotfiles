# shellcheck shell=sh
# shellcheck disable=SC2317

# ~/.config/shell/envvars.sh is loaded by:
# - ~/.config/shell/profile.sh: at login (this is the most important!)
# - ~/.config/shell/rc.sh: in interactive bash and zsh (for convenience in case this file has changed since login)
# - ~/.config/shell/env.sh ONLY IF THIS FILE HASN'T BEEN LOADED ALREADY: whenever zsh starts (for convenience in case dotfiles weren't installed yet when I logged in)

## umask
umask go-rwx

## DOTFILES

if [ -d "$HOME/.dotfiles" ]; then
	DOTFILES="$( (cd "$HOME/.dotfiles" >/dev/null && pwd -P) )"
elif [ -d "$HOME/prog/dotfiles" ]; then
	DOTFILES="$HOME/prog/dotfiles"
elif [ -d "$HOME/dotfiles" ]; then
	DOTFILES="$HOME/dotfiles"
elif [ -d "$HOME/.dotfiles" ]; then
	DOTFILES="$HOME/.dotfiles"
else
	[ -t 0 ] && echo "Unable to locate DOTFILES directory" >&2
	DOTFILES="$HOME/.dotfiles"
fi
export DOTFILES

## XDG Base Directory
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}" # "data that should persist between (application) restarts, but that is not important or portable enough to the user that it should be stored in $XDG_DATA_HOME"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_DIRS="${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
export XDG_CONFIG_DIRS="${XDG_CONFIG_DIRS:-/etc/xdg}"
# XDG_RUNTIME_DIR

## PATH
# shellcheck source=./path.sh
. "$HOME/.config/shell/path.sh"

## EDITOR

# When we're inside Emacs, we want to check if EDITOR, GIT_EDITOR, or VISUAL was
# probably set by Emacs with-editor (https://magit.vc/manual/with-editor.html).
# If so, that's the editor we'll use.
was_set_by_emacs_witheditor() {
	case "$1" in
		*emacsclient*|*WITH-EDITOR*) return 0 ;;
		*) return 1 ;;
	esac
}

my_editor=
if [ -n "$INSIDE_EMACS" ] && was_set_by_emacs_witheditor "$EDITOR"; then
	my_editor="$EDITOR"
elif [ -n "$INSIDE_EMACS" ] && was_set_by_emacs_witheditor "$GIT_EDITOR"; then
	my_editor="$GIT_EDITOR"
elif [ -n "$INSIDE_EMACS" ] && was_set_by_emacs_witheditor "$VISUAL"; then
	my_editor="$VISUAL"
elif [ -n "$INSIDE_EMACS" ] && command -v emacsclient >/dev/null 2>&1; then
	my_editor="emacsclient"
elif [ "$TERM_PROGRAM" = "vscode" ] && command -v code >/dev/null 2>&1; then
	my_editor="code --wait"
elif command -v sw_vers >/dev/null 2>&1 && command -v emacsclient >/dev/null 2>&1 && [ -x "$DOTFILES/bin/emacs-term" ]; then
	my_editor="$DOTFILES/bin/emacs-term"
elif command -v nvim >/dev/null 2>&1; then
	my_editor="nvim"
elif command -v vim >/dev/null 2>&1; then
	my_editor="vim"
elif command -v emacs >/dev/null 2>&1; then
	my_editor="emacs"
elif command -v vi >/dev/null 2>&1; then
	my_editor="vi"
elif command -v mg >/dev/null 2>&1; then
	my_editor="mg"
elif command -v nano >/dev/null 2>&1; then
	my_editor="nano"
elif command -v code >/dev/null 2>&1; then
	my_editor="code --wait"
fi

if [ -n "$my_editor" ]; then
	EDITOR="$my_editor"
	export EDITOR

	# VISUAL="$my_editor"
	# export VISUAL
	unset VISUAL

	# GIT_EDITOR="$my_editor"
	# export GIT_EDITOR
	unset GIT_EDITOR
fi
unset my_editor
unset -f was_set_by_emacs_witheditor

## Pagers

# -R: color
# -i: case-insensitive search unless search string contains uppercase letters
# -x4: tabs are 4 instead of 8
# -z-4: default scrolling window size = screen height - 4
export LESS='-R -i -x4 -z-4'
export PAGER=less
export GIT_PAGER="less -FX"

# bat is called batcat on Ubuntu & Debian
if command -v bat >/dev/null 2>&1 || command -v batcat >/dev/null 2>&1; then
	export MANPAGER="bat_manpager"
	if command -v delta >/dev/null 2>&1; then
		export GIT_PAGER="delta"
	fi
fi

## Misc

# https://aquaproj.github.io/docs/tutorial/global-config
export AQUA_GLOBAL_CONFIG="${AQUA_GLOBAL_CONFIG:-}:${XDG_CONFIG_HOME:-$HOME/.config}/aquaproj-aqua/aqua.yaml"

## AKN_SHELL_ENVVARS_LOADED
export AKN_SHELL_ENVVARS_LOADED=1
