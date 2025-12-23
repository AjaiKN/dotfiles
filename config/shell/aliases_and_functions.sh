# shellcheck shell=bash

## Changing/making/removing directories
alias -- -='cd -'
alias 1='cd -1'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

alias md='mkdir -p'
alias rd=rmdir
# alias d='dirs -v | head -10'
alias po='popd'

# List directory contents
alias lsa='ls -lah'
alias l='ls -lAh'
alias ll='ls -lh'
alias la='ls -lAh'

function superls {
	# shellcheck disable=SC2028
	echo "/ directory \n* executable \n@ symbolic link \n= socket \n% whiteout \n| FIFO"
	ls -AeFGl
}

if [ -e /System ] && command -v gsort >/dev/null 2>&1; then
	alias lsize='gdu -a -h --max-depth=1 | gsort -hr'
else
	alias lsize='du -a -h --max-depth=1 | sort -hr'
fi

if command -v "somo" >/dev/null 2>&1; then
	alias ports='somo'
else
	alias ports='netstat -tuapn' # maybe no -n?
fi

function mkcd {
	mkdir -p "$@" && cd "${@:$#}" || return $?
}

function cdtemp {
	cd "$(mktemp -d "${TMPDIR:-tmp}/${1:-tmp}.XXX")" || return $?
}
# shellcheck disable=SC2139
alias {mktmp,mkcdtemp,mktempcd,cdmktemp,tempcd,tmpcd,tmp}=cdtemp

alias dirvish=dired
alias di=dired

## git aliases
# shellcheck source=./git.sh
. "$HOME/.config/shell/git.sh"

## tar aliases
# shellcheck source=./archives.sh
. "$HOME/.config/shell/archives.sh"

## Misc

alias _='sudo '

#copy file onto clipboard: cat #{file} | pbcopy
#paste onto file: pbpaste > #{file}

#export PERL5LIB="~/.cpan/build/Devel-REPL-1.003025-rohPIt/lib:~/.cpan/build/Moose-2.1206-ddLWA6/lib"

alias spacemacs='emacs-open-new --profile spacemacs'

alias reload_shared='. ${ZDOTDIR:-"$HOME"}/.config/shell/rc.sh'

function cdls { cd "$1" && ls; }

#remember: no space between name and '='
alias edit_shared='vim ${ZDOTDIR:-"$HOME"}/.config/shell/rc.sh; . ${ZDOTDIR:-"$HOME"}/.config/shell/rc.sh'
alias edit_vimrc='vim ~/.vimrc'
alias gitbook='open http://git-scm.com/book/en/v2'
alias matlab-orig='/Applications/MATLAB_*.app/bin/matlab'
alias matlab-nodisp='matlab-orig -nodisplay'
alias matlab='matlab-nodisp -nojvm'
alias stroke-backup='~/prog/stroke-trial-finder/backups/backup.sh'
# brew upgrade casks first, then upgrade everything else (because casks sometimes ask for password)
alias bup='brew upgrade --casks && brew upgrade && brew upgrade --cask wezterm@nightly --greedy-latest'

alias crepl=cling
alias c-repl=cling

# alias occs='ssh -t anelson3@occs.cs.oberlin.edu "tmux attach || tmux"'
alias clyde='occs'
ssh_tmux () {
	OPTIONS="-u"
	# terminals that support tmux control mode
	if [ "$TERM_PROGRAM" = "iTerm2.app" ] || [ "$TERM_PROGRAM" = "WezTerm" ]; then
		if [ -z "$tmux_no_cc" ]; then
			OPTIONS="$OPTIONS -CC"
		fi
	fi
	ssh -t "$@" "env ${tmux_env:-} tmux $OPTIONS attach || env ${tmux_env:-} tmux $OPTIONS new-session"
}
ssh_tmux_occs () {
	tmux_env="SHELL=\$HOME/.local/bin/zsh $tmux_env" ssh_tmux "$@"
}
alias occs="ssh_tmux_occs occs"
alias mining="ssh_tmux_occs mining"
alias cs341="ssh_tmux_occs cs341"
alias cs342="tmux_env='SHELL=/usr/local/bin/zsh' ssh_tmux utm"

alias em="emacs-open"
alias eo="emacs-open"
alias eon="emacs-open-new"
alias et="emacs-term"
alias etn="emacs-term-new"
alias ew="emacs-wait"
alias ewn="emacs-wait-new"
alias er="brew services restart emacs-plus@30"

# alias emacs-plus='EMACS=/opt/homebrew/opt/emacs-plus@30/bin/emacs EMACSDIR=$HOME/.emacs-plus.d /opt/homebrew/opt/emacs-plus@30/bin/emacs --init-directory "$HOME/.emacs-plus.d"'
# alias doom-plus='EMACS=/opt/homebrew/opt/emacs-plus@30/bin/emacs EMACSDIR=$HOME/.emacs-plus.d ~/.emacs-plus.d/bin/doom'
# alias emacs-plus-app='open /opt/homebrew/opt/emacs-plus@30/Emacs.app --env EMACS=$HOMEBREW_PREFIX/opt/emacs-plus@30/bin/emacs --env EMACSDIR=$HOME/.emacs-plus.d --args --init-directory "$HOME/.emacs-plus.d"'

if command -v nvim >/dev/null 2>&1; then
	alias vim=nvim
fi
alias vimregular="/usr/bin/vim"
alias oldvim="/usr/bin/vim"

alias vpn="nocorrect vpn"

alias move="mv -i"
alias copy="cp -i"

# https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/cp/cp.plugin.zsh
alias cpv='rsync -pogbr -hhh --backup-dir="/tmp/rsync-${USERNAME}" -e /dev/null --progress'

alias trash=zap

# ensure that we're using corepack (https://github.com/nodejs/corepack, https://nodejs.org/api/corepack.html)
# This shouldn't actually be necessary.
# alias yarn="corepack yarn"
# alias yarnpkg="corepack yarnpkg"
# alias pnpm="corepack pnpm"
# alias pnpx="corepack pnpx"
# alias npm="corepack npm"
# alias npx="corepack npx"

iterm() {
	open -a iTerm .
}

# kills process at port (can supply multiple seperated by commas)
killport() {
	lsof -ti:"$1" -sTCP:LISTEN | xargs kill -15
	# todo: kill -9 if it doesn't work
}

# NOTE: Use `killall process_name` to kill all processes with a given name.

# lazy load GitHub Copilot CLI
function double_question_mark_gh_copilot() {
	unalias "??"
	eval "$(github-copilot-cli alias -- "$0")"
	copilot_what-the-shell "$@"
}
alias "??"="double_question_mark_gh_copilot";

# 1. co # opens vscode in current dir
# 2. co thing/file # opens vscode in current dir, then opens file
function co() {
	if [ -z "$1" ]; then
		code .
	else
		code . && code "$1"
	fi
}

alias o='a -e open'
function finder() {
	if [ "$1" != "" ]
	then
		if [[ -d "$1" ]]
		then
			open "$1"
		else
			open -R "$1"
		fi
	else
		open .
	fi;
}

function ungit() {
	if [ -z "$1" ]
	then
		npm start --prefix "$HOME/prog/obiesource/ungit" -- --forcedLaunchPath "$(realpath .)"
	else
		npm start --prefix "$HOME/prog/obiesource/ungit" -- --forcedLaunchPath "$(realpath "${1/#\~/$HOME}")"
	fi
}
function ungit-open() {
	ungit "$1" > /dev/null &
}

# cd to the directory of a file
# https://unix.stackexchange.com/a/76229
function cdf() {
	if [ -d "$1" ] || [ -z "$1" ]; then
		builtin cd "$@" || return 1
	else
		builtin cd "${1%/*}" || return 1
	fi
}

# Rerun previous command under sudo
alias please='sudo $(fc -ln -1)'

# not very reliable
# https://apple.stackexchange.com/questions/20547/how-do-i-find-my-ip-address-from-the-command-line
# external: curl https://ifconfig.me
#        or dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com
alias ip='ipconfig getifaddr en0'

alias nvim-kickstart='NVIM_APPNAME="nvim-kickstart" nvim'

alias c='a -e code'
