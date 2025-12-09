# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
#
# .zshenv is sourced on ALL invocations of the shell, unless the -f option is
# set.  It should NOT normally contain commands to set the command search path,
# or other common environment variables unless you really know what you're
# doing.  E.g. running "PATH=/custom/path gdb program" sources this file (when
# gdb runs the program via $SHELL), so you want to be sure not to override a
# custom environment in such cases.  Note also that .zshenv should not contain
# commands that produce output or assume the shell is attached to a tty.
#

if [[ -o interactive ]]; then
	### choosing a theme
	# version >= 5.1
	if [[ $ZSH_VERSION == (5.<1->*|<6->.*) && $TERM != dumb && -z $RSTUDIO && ( -z $INSIDE_EMACS || $INSIDE_EMACS == vterm* ) ]]; then
		ZSH_THEME="powerlevel10k/powerlevel10k"
	else
		ZSH_THEME="simple"
	fi

	if [ -t 0 ] && [ -t 1 ] && [ -t 2 ]; then
		### Cursor style
		# immediately change cursor style so that it looks like insert mode when the Powerlevel10k instant prompt starts
		# (6 = non-blinking bar cursor)
		echo -ne '\e[6 q'

		### Powerlevel10k instant prompt
		# EXPERIMENTAL: load powerlevel10k instant prompt super early
		if [[ $ZSH_THEME == "powerlevel10k/powerlevel10k" ]]; then
			# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
			# Initialization code that may require console input (password prompts, [y/n]
			# confirmations, etc.) must go above this block; everything else may go below.
			if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
				source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
			fi
		fi
	fi
fi

if [[ -o interactive ]] && ! (( $+functions[_git] )); then
	# make sure we use the git completion that ships with zsh, which is better than the one that ships with git
	autoload -Uzr _git
fi

source "$HOME/.config/shell/shared/env.sh"

# from zsh4humans
if [[ $EUID == 0 ]]; then # && -z ~(#qNU) && $DOTFILES == ~/* ]]; then
	typeset -gri _akn_dangerous_root=1
else
	typeset -gri _akn_dangerous_root=0
fi

# This kludge can be used to override some installations that put aliases for
# rm, mv, etc. into the system profiles.  Just be sure to put "unalias alias"
# in your own rc file(s) if you use this.
# alias alias=:

# Some people insist on setting their PATH here to affect things like ssh.
# Those that do should probably use $SHLVL to ensure that this only happens
# the first time the shell is started (to avoid overriding a customized
# environment).  Also, the various profile/rc/login files all get sourced
# *after* this file, so they will override this value.  One solution is to
# put your path-setting code into a file named .zpath, and source it from
# both here (if we're not a login shell) and from the .zprofile file (which
# is only sourced if we are a login shell).
# if [[ $SHLVL == 1 && ! -o LOGIN ]]; then
# 	source ~/.zpath
# fi
