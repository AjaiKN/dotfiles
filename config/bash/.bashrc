# shellcheck shell=bash
# shellcheck disable=SC2317

source "$HOME/.config/shell/env.sh"

# https://planet.kde.org/siosms-blog-2023-12-21-dont-change-your-login-shell-use-a-modern-terminal-emulator/
# Only trigger if:
if
	# - This shell is interactive
	[[ $- == *i* ]] &&
		# # - We're not in a login shell
		# ! shopt -q login_shell &&
		# - AKN_INSIDE_ZSH isn't defined (so that `exec bash` from inside zsh doesn't trigger it)
		[[ -z ${AKN_INSIDE_ZSH:-} ]] &&
		# - We did not call: bash -c '...'
		[[ -z ${BASH_EXECUTION_STRING:-} ]] &&
		# - The zsh binary exists and is executable
		command -v zsh >/dev/null 2>&1 &&
		# - 'zsh' is not the parent process of this shell
		grep -qv 'zsh' "/proc/$PPID/comm" 2>/dev/null &&
		akn_ps_output="$(ps --no-header --pid=$PPID --format=comm 2>/dev/null)" && [[ $akn_ps_output != "zsh" ]]
then
	export AKN_INSIDE_ZSH=1
	exec zsh
fi

# Loaded in interactive non-login shells in bash.
# On Mac, interactive shells are login shells by default, so we load this file in ~/.bash_profile.
# If you run the `bash` command directly, it'll load this file directly. (Use `bash -l` to start a login shell and load ~/.bash_profile.)
# (So effectively, this file is loaded in all interactive shells in bash.)

echo_if_interactive() {
	if [ -t 0 ]; then
		echo "$@"
	fi
}

# echo_if_interactive "Loading ~/.bashrc"

## Nix prologue
OLD_PATH_BASHRC=$PATH

## If not running interactively, don't do anything
# case $- in
#     *i*) ;;
#       *) return;;
# esac

## disable command-not-found handlers

unfunction command_not_found_handle 2>/dev/null
unfunction command_not_found_handler 2>/dev/null

## History
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# make HISTSIZE and HISTFILESIZE as large as possible
# https://github.com/ohmybash/oh-my-bash/blob/2651f35600fbf3452b1d1652ccfb709b1a1692d7/lib/history.sh#L36-L74
export HISTSIZE=$((0x7FFF7FFF))
export HISTFILESIZE=$((0x7FFF7FFF))

### bash-sensible: history
# https://github.com/mrzool/bash-sensible/blob/master/sensible.bash
# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
[[ $- = *i* ]] && bind Space:magic-space
# Save multi-line commands as one command
shopt -s cmdhist
# Record each line as it gets issued
PROMPT_COMMAND='history -a'
# Don't record some commands
# https://news.ycombinator.com/item?id=39083708
export HISTIGNORE="exec env*:history*:&:[ ]*:exit:ls:bg:fg:history:clear"
# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

### HISTFILE
# https://news.ycombinator.com/item?id=33186412
export HISTFILE="$HOME/.bash_my_history"

## Settings
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# https://github.com/ohmybash/oh-my-bash/blob/master/lib/shopt.sh
# Turn on extended glob patterns such as @(...), *(...), ?(...), and +(...)
shopt -s extglob 2> /dev/null

# Turn on recursive globbing
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar 2> /dev/null

# https://github.com/mrzool/bash-sensible/blob/master/sensible.bash
# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

## bash-sensible: better directory navigation
# https://github.com/mrzool/bash-sensible/blob/master/sensible.bash

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH="."

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars

# Examples:
# export dotfiles="$HOME/dotfiles"
# export projects="$HOME/projects"
# export documents="$HOME/Documents"
# export dropbox="$HOME/Dropbox"

## completion
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
	if [ -f /usr/share/bash-completion/bash_completion ]; then
		# shellcheck source=/dev/null
		. /usr/share/bash-completion/bash_completion
	elif [ -f /etc/bash_completion ]; then
		# shellcheck source=/dev/null
		. /etc/bash_completion
	fi
fi

### bash-sensible: completion bindings
# https://github.com/mrzool/bash-sensible/blob/master/sensible.bash
if [[ $- = *i* ]]; then
	# Perform file completion in a case insensitive fashion
	bind "set completion-ignore-case on"
	# Treat hyphens and underscores as equivalent
	bind "set completion-map-case on"
	# Display matches for ambiguous patterns at first tab press
	bind "set show-all-if-ambiguous on"
	# Immediately add a trailing slash when autocompleting symlinks to directories
	bind "set mark-symlinked-directories on"
fi

## lesspipe
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

## prompt
# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
		debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
		xterm-color|*-256color|wezterm*) color_prompt=yes;;
esac

if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
fi


if [ -n "$color_prompt" ]; then
	PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
	PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# https://github.com/mrzool/bash-sensible/blob/master/sensible.bash
# Automatically trim long paths in the prompt (requires Bash 4.x)
# PROMPT_DIRTRIM=2

## title
# If this is an xterm set the title to user@host:dir
case "$TERM" in
	xterm*|rxvt*)
		PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
		;;
	*)
		;;
esac

## aliases, ls colors
# # enable color support of ls and also add handy aliases
# if [ -x /usr/bin/dircolors ]; then
# 		test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
# 		alias ls='ls --color=auto'
# 		#alias dir='dir --color=auto'
# 		#alias vdir='vdir --color=auto'

# 		alias grep='grep --color=auto'
# 		alias fgrep='fgrep --color=auto'
# 		alias egrep='egrep --color=auto'
# fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

# if [ -f ~/.bash_aliases ]; then
#     . ~/.bash_aliases
# fi

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

alias zcalc='zsh -ilc zcalc'

## fzf
if command -v fzf >/dev/null 2>&1; then
	# Set up fzf key bindings and fuzzy completion
	eval "$(fzf --bash)"
fi

## fasd
if command -v fasd >/dev/null 2>&1; then
	fasd_cache="${XDG_CACHE_HOME:-$HOME/.cache}/fasd-init-bash"
	if [ "$(command -v fasd)" -nt "$fasd_cache" ] || [ ! -s "$fasd_cache" ]; then
		fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
	fi
	source "$fasd_cache"
	unset fasd_cache
fi

## ~/.config/shell/rc.sh
source "$HOME/.config/shell/rc.sh"

## mise
unset __MISE_ORIG_PATH # Leaving this set has resulted in some confusing behavior sometimes
# hash mise 2>/dev/null && eval "$(mise activate bash)"

## tabtab completions
# https://github.com/mklabs/tabtab
# Netlify CLI uses this
[ -f ~/.config/tabtab/bash/__tabtab.bash ] && . ~/.config/tabtab/bash/__tabtab.bash

## Emacs vterm integration
if [[ "$INSIDE_EMACS" = 'vterm' ]] && [[ -n ${EMACS_VTERM_PATH} ]] && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
	# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration-files
	source "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
fi

# https://stackoverflow.com/questions/44534678/how-to-change-cursor-shape-depending-on-vi-mode-in-bash
# https://stackoverflow.com/a/8363532
case "$TERM" in
	"dumb")
		:
		;;
	xterm*|rxvt*|eterm*|screen*|wezterm*)
		PS0="\e[2 q\2"
		;;
	*)
		:
		;;
esac

[ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ] && . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

## direnv

if command -v direnv >/dev/null 2>&1; then
	eval "$(direnv hook bash)"
fi

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_BASHRC:$PATH
fi

# echo_if_interactive "Done loading ~/.bashrc"

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

## Guix

. "$HOME/.config/shell/guix.sh"

### Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
	if [[ $PS1 =~ (.*)"\\$" ]]; then
		PS1="${BASH_REMATCH[1]} [env]\\\$ "
	fi
fi

