# shellcheck shell=bash

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
