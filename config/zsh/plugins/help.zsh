# The `run-help` (aliased to `help`) command is a zsh builtin that
# includes help text for zsh builtins and functions.
# https://postgresqlstan.github.io/cli/zsh-run-help/
# https://wiki.archlinux.org/title/zsh#Help_command
# also see help zshcontrib

# export HELPDIR="/usr/share/zsh/$(zsh --version | cut -d' ' -f2)/help"
# https://github.com/zimfw/run-help/blob/master/init.zsh
if (( ! ${+HELPDIR} )); then
	local dir
	for dir in /usr/local/share/zsh/help /usr/share/zsh/${ZSH_VERSION}/help /usr/share/zsh/help; do
		if [[ -d ${dir} ]]; then
			typeset -g HELPDIR=${dir}
			break
		fi
	done
	unset dir
fi

unalias run-help 2>/dev/null       # don't display err if already done
autoload -Uz run-help              # load the function
autoload -Uz run-help-ip run-help-openssl run-help-p4 run-help-sudo run-help-svk run-help-svn # run-help-git
run-help-git () {
	if [[ $1 == branchless ]]; then
		if [[ $# -lt 2 ]]; then
			git help branchless
		else
			man git-branchless-$2
		fi
	else
		git help ${1:-git}
	fi
}
alias help=run-help                # optionally alias run-help to help

# press alt-h or esc-h or control-h after typing a command to see help for that command
if [ "$TERM" != "eterm-color" ]; then
	bindkey "^[H" run-help
	bindkey "^[h" run-help
	bindkey "^H" run-help
	bindkey "^h" run-help
	bindkey "ESC-h" run-help
	bindkey "ESC-H" run-help
	# or K in vi normal mode
	bindkey -M vicmd 'K' run-help
fi

bash-help() {
	bash -c "help $@" | less
}

# Some help methods: man; run-help (help); info; tldr; curl cheat.sh/<command>

## which (equivalent in zsh to whence -c)
# Fedora creates an alias for clippaste, which is helpful in bash but not in zsh.
unalias which 2>/dev/null

my_which() {
	# If it's more than one thing, show all of them.
	builtin whence -av $@
	local ret=$?
	# If it's a function, show both the source location and the definition.
	builtin typeset -f $@
	return $ret
}
alias which=my_which
