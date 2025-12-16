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

## git aliases

#from ohmyzsh

# Get the default branch name from common branch names or fallback to remote HEAD
function git_main_branch() {
	command git rev-parse --git-dir &>/dev/null || return

	local remote ref

	for ref in refs/{heads,remotes/{origin,upstream}}/{main,trunk,mainline,default,stable,master}; do
		if command git show-ref -q --verify "$ref"; then
			echo "${ref:t}"
			return 0
		fi
	done

	# Fallback: try to get the default branch from remote HEAD symbolic refs
	for remote in origin upstream; do
		ref=$(command git rev-parse --abbrev-ref $remote/HEAD 2>/dev/null)
		if [[ $ref == $remote/* ]]; then
			echo "${ref#"$remote/"}"; return 0
		fi
	done

	# If no main branch was found, fall back to master but return error
	echo master
	return 1
}

alias ggpur='ggu'
alias g='git'
alias ga='git add'
alias gaa='git add --all'
alias gapa='git add --patch'
alias gau='git add --update'
alias gav='git add --verbose'
alias gwip='git add -A && git commit --no-verify --no-gpg-sign --message "--wip-- [skip ci]"'
alias gam='git am'
alias gama='git am --abort'
alias gamc='git am --continue'
alias gamscp='git am --show-current-patch'
alias gams='git am --skip'
alias gap='git apply'
alias gapt='git apply --3way'
alias gbs='git bisect'
alias gbsb='git bisect bad'
alias gbsg='git bisect good'
alias gbsn='git bisect new'
alias gbso='git bisect old'
alias gbsr='git bisect reset'
alias gbss='git bisect start'
alias gbl='git blame -w'
alias gb='git branch'
alias gba='git branch --all'
alias gbd='git branch --delete'
alias gbD='git branch --delete --force'
# shellcheck disable=SC2142
alias gbgd='LANG=C git branch --no-color -vv | grep ": gone\]" | cut -c 3- | awk '"'"'{print $1}'"'"' | xargs git branch -d'
# shellcheck disable=SC2142
alias gbgD='LANG=C git branch --no-color -vv | grep ": gone\]" | cut -c 3- | awk '"'"'{print $1}'"'"' | xargs git branch -D'
alias gbm='git branch --move'
alias gbnm='git branch --no-merged'
alias gbr='git branch --remote'
alias gbg='LANG=C git branch -vv | grep ": gone\]"'
alias gco='git checkout'
alias gcor='git checkout --recurse-submodules'
alias gcb='git checkout -b'
alias gcB='git checkout -B'
alias gcm='git checkout $(git_main_branch)'
# alias gcp='git cherry-pick'
alias gcpa='git cherry-pick --abort'
alias gcpc='git cherry-pick --continue'
alias gclean='git clean --interactive -d'
alias gcl='git clone --recurse-submodules'
alias gclf='git clone --recursive --shallow-submodules --filter=blob:none --also-filter-submodules'
alias gcam='git commit --all --message'
alias gcas='git commit --all --signoff'
alias gcasm='git commit --all --signoff --message'
alias gcs='git commit --gpg-sign'
alias gcss='git commit --gpg-sign --signoff'
alias gcssm='git commit --gpg-sign --signoff --message'
alias gcmsg='git commit --message'
alias gcsm='git commit --signoff --message'
alias gc='git commit --verbose'
alias gca='git commit --verbose --all'
alias gca!='git commit --verbose --all --amend'
alias gcan!='git commit --verbose --all --no-edit --amend'
alias gcans!='git commit --verbose --all --signoff --no-edit --amend'
alias gcann!='git commit --verbose --all --date=now --no-edit --amend'
alias gc!='git commit --verbose --amend'
alias gcn='git commit --verbose --no-edit'
alias gcn!='git commit --verbose --no-edit --amend'
alias gcf='git config --list'
alias gdct='git describe --tags $(git rev-list --tags --max-count=1)'
alias gcfu='git commit --fixup'
alias gd='git diff'
alias gdca='git diff --cached'
alias gdcw='git diff --cached --word-diff'
alias gds='git diff --staged'
alias gdw='git diff --word-diff'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gf='git fetch'
alias gfa='git fetch --all --tags --prune --jobs=10'
alias gfo='git fetch origin'
alias gg='git gui citool'
alias gga='git gui citool --amend'
alias ghh='git help'
alias glgg='git log --graph'
alias glgga='git log --graph --decorate --all'
alias glgm='git log --graph --max-count=10'
alias glods='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset" --date=short'
alias glod='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset"'
alias glola='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset" --all'
alias glols='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset" --stat'
alias glol='git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset"'
alias glo='git log --oneline --decorate'
alias glog='git log --oneline --decorate --graph'
alias gloga='git log --oneline --decorate --graph --all'
alias gl='git pull'
# alias gpr='git pull --rebase'
alias gprv='git pull --rebase -v'
alias gpra='git pull --rebase --autostash'
alias gprav='git pull --rebase --autostash -v'
alias glg='git log --stat'
alias glgp='git log --stat --patch'
alias gignored='git ls-files -v | grep "^[[:lower:]]"'
alias gfg='git ls-files | grep'
alias gm='git merge'
alias gma='git merge --abort'
alias gmc='git merge --continue'
alias gms="git merge --squash"
alias gmff="git merge --ff-only"
alias gmom='git merge origin/$(git_main_branch)'
alias gmum='git merge upstream/$(git_main_branch)'
alias gmtl='git mergetool --no-prompt'
alias gmtlvim='git mergetool --no-prompt --tool=vimdiff'
alias gl='git pull'
alias gpr='git pull --rebase'
alias gprv='git pull --rebase -v'
alias gpra='git pull --rebase --autostash'
alias gprav='git pull --rebase --autostash -v'
alias gprom='git pull --rebase origin $(git_main_branch)'
alias gpromi='git pull --rebase=interactive origin $(git_main_branch)'
alias gprum='git pull --rebase upstream $(git_main_branch)'
alias gprumi='git pull --rebase=interactive upstream $(git_main_branch)'
alias glum='git pull upstream $(git_main_branch)'
alias gp='git push'
alias gpf!='git push --force'
alias gpf='git push --force-with-lease --force-if-includes'
alias gpn='git push --dry-run'
alias gpv='git push --verbose'
alias gpoat='git push origin --all && git push origin --tags'
alias gpod='git push origin --delete'
alias gpu='git push upstream'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase --interactive'
alias grbo='git rebase --onto'
alias grbs='git rebase --skip'
alias grbm='git rebase $(git_main_branch)'
alias grbom='git rebase origin/$(git_main_branch)'
alias grbum='git rebase upstream/$(git_main_branch)'
alias grf='git reflog'
alias gr='git remote'
alias grv='git remote --verbose'
alias gra='git remote add'
alias grrm='git remote remove'
alias grmv='git remote rename'
alias grset='git remote set-url'
alias grup='git remote update'
alias grh='git reset'
alias gru='git reset --'
alias grhh='git reset --hard'
alias grhk='git reset --keep'
alias grhs='git reset --soft'
alias gpristine='git reset --hard && git clean --force -dfx'
alias gwipe='git reset --hard && git clean --force -df'
alias grs='git restore'
alias grss='git restore --source'
alias grst='git restore --staged'
alias gunwip='git rev-list --max-count=1 --format="%s" HEAD | grep -q "\--wip--" && git reset HEAD~1'
alias grev='git revert'
alias greva='git revert --abort'
alias grevc='git revert --continue'
# alias grm='git rm' # conflicts with GNU rm on Mac
alias grmc='git rm --cached'
alias gcount='git shortlog --summary --numbered'
alias gsh='git show'
alias gsps='git show --pretty=short --show-signature'
alias gstash='git stash'
alias gstall='git stash --all'
alias gstaa='git stash apply'
alias gstc='git stash clear'
alias gstd='git stash drop'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gstpo='git stash pop'
alias gsto='git stash pop'
alias gstpop='git stash pop'
alias gsta='git stash push'
alias gstpu='git stash push'
alias gstpus='git stash push'
alias gstpush='git stash push'
alias gstu='git stash push --include-untracked'
alias gsts='git stash show --patch'
alias gst='git status'
alias gss='git status --short'
alias gsb='git status --short --branch'
alias gsi='git submodule init'
alias gsu='git submodule update'
alias gsd='git svn dcommit'
alias gsr='git svn rebase'
alias gsw='git switch'
alias gswc='git switch --create'
alias gswm='git switch $(git_main_branch)'
alias gta='git tag --annotate'
alias gts='git tag --sign'
alias gtv='git tag | sort -V'
alias gignore='git update-index --assume-unchanged'
alias gunignore='git update-index --no-assume-unchanged'
alias gwch='git whatchanged -p --abbrev-commit --pretty=medium'
alias gwt='git worktree'
alias gwta='git worktree add'
alias gwtls='git worktree list'
alias gwtmv='git worktree move'
alias gwtrm='git worktree remove'
alias gk='\gitk --all --branches &!'
alias gke='\gitk --all $(git log --walk-reflogs --pretty=%h) &!'
alias g.='cd "$(git rev-parse --show-toplevel || printf ".\n")"'
alias g..='cd "$(git -C .. rev-parse --show-toplevel || printf ".\n")"'

# based on magit
alias gb='git branch'
alias gbb='git switch'
alias gbc='git switch -c'
alias gbm='git branch -m'
alias gbd='git branch -d'
alias gbk='git branch -d'
alias gB='git bisect'
alias gBB='git bisect start'
alias gBs='git bisect run'
alias gBb='git bisect bad'
alias gBg='git bisect good'
alias gBk='git bisect skip'
alias gBr='git bisect reset'
alias gc='git commit'
alias gce='git extend'
alias gca='git commit --amend'
alias gco='git checkout'
alias gC='git clone'
alias gCs='git clone --depth=1'
alias gd='git diff'
# alias gdu='git diff'
alias gds='git diff --staged'
alias gdw='git diff HEAD'
alias gdc='git show'
alias gdt='git stash show'
alias gf='git fetch'
alias gF='git pull'
# alias gh='git help'
alias gi='git ignore'
alias gI='git init'
alias gl='git log'
alias glb='git log --branches --remotes'
alias gla='git log --all'
alias glr='git reflog'
# alias gls='git shortlog'
# alias gm='git merge'
alias gme='git merge --edit'
alias gmn='git merge --no-commit'
alias gM='git remote'
alias gMa='git remote add'
alias gMr='git remote rename'
alias gMk='git remote remove'
alias gMp='git remote prune'
alias gp='git push'
alias gr='git rebase'
alias gt='git tag'
alias gT='git notes'
alias gv='git revert'
alias gw='git apply'
alias gW='git format-patch'
alias gO='git reset'
alias gOm='git reset --mixed'
alias gOs='git reset --soft'
alias gOh='git reset --hard'
alias gOk='git reset --keep'
alias gZ='git stash'
alias gZz='git stash push'
alias gZi='git stash push --staged'
alias gZw='git stash-unstaged'
alias gZa='git stash apply'
alias gZp='git stash pop'
alias gZk='git stash drop'
alias gZl='git stash list'
alias gZv='git stash show'
alias gZb='git stash branch'

## tar aliases

# also see ohmyzsh `takeurl` (or just `take`)

# Usage:
#   untar/unarchive FILE [OPTIONS...]
#   mktar,tar-create,archive,archive-create DESTINATION [OPTIONS...] SOURCEFILES
#   tar-ls,archive-ls FILE [OPTIONS...]
#
# bsdtar is better when available.
# - bsdtar "can extract from tar, pax, cpio, zip, jar, ar, xar, rpm, 7-zip, and
#   ISO 9660 cdrom images and can create tar, pax, cpio, ar, zip, 7-zip, and
#   shar archives."
# - bsdtar has a good default when creating archives: restricted pax format,
#   "which will create ustar archives except for entries that require pax
#   extensions (for long filenames, ACLs, etc)."
#
# shellcheck disable=SC2139
if command -v bsdtar >/dev/null 2>&1; then
	alias tar=bsdtar
	alias {untar,unarchive}='bsdtar -xf'
	alias {mktar,tar-create,archive,archive-create}='bsdtar -caf'
	alias {tar-ls,archive-ls}='bsdtar -tvf'
else
	# e[x]tract [f]ile
	alias {untar,unarchive}='tar -xf'
	# [c]reate a [f]ile, with format determined [a]utomatically from destination file extension
	# (GNU tar uses the gnu format by default, so make sure it uses the more compatible and more flexible pax format)
	alias {mktar,tar-create,archive,archive-create}='tar -c --format=pax -af'
	# list files in archive
	alias {tar-ls,archive-ls}='tar -tvf'
fi

if command -v wget >/dev/null 2>&1; then
	alias download='wget'
else
	alias download='curl -O -L'
fi

function download-extract {
	local COLOR='\033[1;36m'
	local NOCOLOR='\033[0m'
	local cmd=tar
	if [[ $1 =~ .(tar|pax|cpio|zip|jar|ar|xar|pkg|xip|rpm|7z|iso|iso9660)$ ]]; then
		# bsdtar "can extract from tar, pax, cpio, zip, jar, ar, xar, rpm, 7-zip, and ISO 9660 cdrom images"
		cmd=bsdtar
	fi
	local outdir=${2:-output}
	if command -v "$cmd" >/dev/null 2>&1; then
		# first argument: URL
		# second argument: directory to put it in (to avoid tar-bombing)
		mkdir "$outdir" || return 1
		curl -L "$1" | "$cmd" xf - -C "$outdir" || return 1
		echo -e "Extracted into $COLOR$(realpath "$outdir")/$NOCOLOR ($(du -sh "$outdir" | cut -f -1)):"
		ls -Al "$outdir"
	else
		echo "command $cmd not found"
		return 1
	fi
}

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
