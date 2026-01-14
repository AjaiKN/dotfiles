#License for this file:
# MIT License
#
# Copyright (c) 2020 Roman Perepelitsa
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

() {
	emulate -L zsh &&
		setopt typeset_silent pipe_fail extended_glob prompt_percent no_prompt_subst &&
		setopt no_prompt_bang no_bg_nice no_aliases

	# https://github.com/umlx5h/zsh-manpage-completion-generator
	# This should be at the end of fpath.
	fpath+=($HOME/.local/share/zsh/generated_man_completions)
	# make sure my stuff is at the front
	add_to_fpath "$ZSH_CACHE_DIR/completions" "$ZSH_CUSTOM/completions" "$ZSH_CUSTOM/functions"

	autoload -Uz is-at-least compinit
	# from https://github.com/zimfw/completion/blob/master/init.zsh
	if (( ${+_comps} )); then
		# https://github.com/zimfw/zimfw/wiki/Troubleshooting#completion-is-not-working
		print -u2 'warning: __compinit.zsh: completion was already initialized. Will call compinit again.'
	fi

	unfunction compdef

	local -aU editors=(
		vi vim nvim emacs nano gedit code kak kate mcedit joe $EDITOR $VISUAL
		bat cat less more $PAGER)
	zstyle ':completion:*:*:('${(j:|:)editors}'):*:*' ignored-patterns '*.zwc'

	zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"

	if (( !_akn_dangerous_root )) && zstyle -t ':completion::complete:' use-cache; then
		local cache
		zstyle -s ':completion::complete:' cache-path cache
		: ${cache:=${ZDOTDIR:-~}/.zcompcache}
		if [[ ! -e $cache ]]; then
			zf_mkdir -m 0700 -p -- $cache
		fi
	fi

	local zdumpfile=$ZSH_CACHE_DIR/zcompdump-z4h-$EUID-$ZSH_VERSION

	local -a stat files=(${^fpath}/^([^_]*|*~|*.zwc)(-.N))
	(( ! $#files )) || zstat -A stat +mtime -- $files
	local real_sig=($ZSH_VERSION $ZSH_PATCHLEVEL $files $stat)
	real_sig='# '${(V)${(pj:\0:)real_sig}}$'\n'

	local sig
	if [[ -r $zdumpfile ]] &&
			sysread -s $#real_sig sig <$zdumpfile        &&
			[[ $sig == $real_sig && -r $zdumpfile.zwc ]] &&
			zstat -A stat +mtime -- $zdumpfile $zdumpfile.zwc &&
			(( stat[2] == stat[1] + 1 )); then
		compinit -C -d $zdumpfile
	else
		echo "Creating new dumpfile"
		local tmp=$ZSH_CACHE_DIR/tmp.zcompdump-z4h.$sysparams[pid]
		zf_rm -f -- $zdumpfile $zdumpfile.zwc $tmp $tmp.2
		compinit -C -d $tmp
		{ print -rn -- $real_sig; <$tmp } >$tmp.2
		zf_rm -f -- $tmp
		zf_mv -- $tmp.2 $zdumpfile
		zsh_compile $zdumpfile
	fi

	# Replay compdef calls.
	local args
	for args in $_akn_compdef; do
		compdef "${(@0)args}"
	done
	unset _akn_compdef

	# local cmd
	# for cmd in helm kitty kubectl oc; do
	# 	# Homebrew ships broken completions for kubectl, so we use our own even
	# 	# if _comps[kubectl] is set.
	# 	#
	# 	# TODO: what about the rest of them in the list? Move them below?
	# 	[[ -v commands[$cmd] ]] && compdef -- -z4h-complete-$cmd $cmd
	# done

	# for cmd in cargo bw gh rustup; do
	# 	if [[ -v commands[$cmd] && ! -v _comps[$cmd] ]]; then
	# 		compdef -- -z4h-complete-$cmd $cmd
	# 	fi
	# done

	# for cmd in terraform vault packer; do
	# 	if [[ -v commands[$cmd] && ! -v _comps[$cmd] ]]; then
	# 		complete -o nospace -C =$cmd $cmd
	# 	fi
	# done

	# if [[ -v commands[aws_completer] && ! -v _comps[aws] ]]; then
	# 	complete -C =aws_completer aws
	# fi

	# if [[ -v commands[gcloud] && ! -v _comps[gcloud] ]]; then
	# 	local dirs=(
	# 		${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/share/google-cloud-sdk}
	# 		~/google-cloud-sdk
	# 		/usr/share/google-cloud-sdk
	# 		/snap/google-cloud-sdk/current
	# 		/snap/google-cloud-cli/current
	# 		/usr/lib/google-cloud-sdk
	# 		/usr/lib64/google-cloud-sdk
	# 		/opt/google-cloud-sdk
	# 		/opt/local/libexec/google-cloud-sdk
	# 	)
	# 	source -- $^dirs/completion.zsh.inc(-.Ne'<[[ -r ${REPLY:a} ]]>') /dev/null
	# fi

	# from https://github.com/zimfw/completion/blob/master/init.zsh
	functions[compinit]=$'print -u2 \'warning: compinit being called again after completion module at \'${funcfiletrace[1]}
	'${functions[compinit]}
}
