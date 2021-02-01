function .set_zsh_plugin_default_branch {
	local default_branch=$(curl -L -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" "https://api.github.com/repos/${1}" | jq -r .default_branch)
	if [ -n "$default_branch" ] && [ "$default_branch" != null ]; then
		set -x
		git -C "$DOTFILES" submodule set-branch -b "$default_branch" -- "config/zsh/plugins/${1}"
		set +x
	fi
}

function install_zsh_plugin {
	local url="https://github.com/${1}"
	set -x
	mkdir -p "$DOTFILES/config/zsh/plugins/${1:h}"
	git -C "$DOTFILES" submodule add --depth=1 "$url" "config/zsh/plugins/${1}"
	set +x
	.set_zsh_plugin_default_branch "$1"
}

function uninstall_zsh_plugin {
	set -x
	git -C "$DOTFILES" rm -f "config/zsh/plugins/${1}"
	set +x
}

function update_zsh_plugin {
	.set_zsh_plugin_default_branch "$1"
	set -x
	git -C "$DOTFILES" submodule update --remote --depth=1 "config/zsh/plugins/${1}"
	set +x
}

function add_to_fpath {
	[ "$#" -eq 0 ] && return 1
	for dir in "$@"; do
		if [ -d "$dir" ]; then
			fpath=("$dir" $fpath)
	 	fi
	done
}
function add_to_path {
	[ "$#" -eq 0 ] && return 1
	for dir in "$@"; do
		if [ -d "$dir" ]; then
			path=("$dir" $path)
	 	fi
	done
}

function load_plugin {
	local possible_paths plugin_file
	possible_paths=(
		# ":t" gets the basename, in case the plugin name includes the full repo name
		"$ZSH_PLUGINS/${1}/${1:t}.plugin.zsh"(.N) # regular plugins, ohmyzsh plugins
		"$ZSH_PLUGINS/${1}/init.zsh"(.N) # prezto modules, zim modules
		"$ZSH_PLUGINS/${1}.zsh"(.N)
		"$ZSH_PLUGINS/${1}.plugin.zsh"(.N)
		"$ZSH_PLUGINS/${1}"/*.plugin.zsh(.N)
		"$ZSH_PLUGINS/${1}"/*.zsh(.N)
	)
	if [ ${#possible_paths[@]} -eq 0 ]; then
		>&2 echo "Plugin not found: ${1}"
		>&2 echo "To add it as a submodule, run:"
		>&2 echo "install_zsh_plugin ${1}"
		>&2 echo
	else
		zsh_loaded_plugins+=( "${1:t}" )
		plugin_file="${possible_paths[@]:0:1}"
	  typeset -F start_time=EPOCHREALTIME
		# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#zero-handling
		ZERO="$plugin_file" compile_and_source "$plugin_file"
		typeset -F end_time=EPOCHREALTIME
		plugin_times+=( $((end_time - start_time)) )
	fi
}

# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#indicator
typeset -ga zsh_loaded_plugins

typeset -ga plugin_times

function print_plugin_times {
	(
		local count=${#plugins[@]}
		for i in $(seq 1 $count); do
			printf '%7d %s\n' $(( 1000000 * ${plugin_times[$i]} )) "${plugins[$i]}"
		done
	) | sort -n --reverse
}

plugins=()
function plugin {
	plugins+=("$1")
}

function load_plugins {
	# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#pmspec
	export PMSPEC=0fbis

	unset -f plugin

	# make sure we use the git completion that ships with zsh, which is better than the one that ships with git
	autoload -Uzr _git

	mkdir -p "$ZSH_CACHE_DIR/completions"
	add_to_fpath "$ZSH_CACHE_DIR/completions" "$ZSH_CUSTOM/completions" "$ZSH_CUSTOM/functions"

	# setup fpath and path
	for plugin in $plugins; do
		# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#funcs-dir
		if ! add_to_fpath "$ZSH_PLUGINS/${plugin}/functions"(/N); then
	 		add_to_fpath "$ZSH_PLUGINS/${plugin}"(/N)           # omz modules
			add_to_fpath "$ZSH_PLUGINS/${plugin}/src"(/N)       # zsh-users/zsh-completions
		fi
		# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#bin-dir
		add_to_path "$ZSH_PLUGINS/${plugin}/bin"(/N)
	done

	# actually load the plugins
	for plugin in $plugins; do
		load_plugin "$plugin"
	done
	unset plugin

	# load the theme
	if [[ -n "$ZSH_THEME" ]]; then
		compile_and_source "$ZSH_CUSTOM/themes/$ZSH_THEME.zsh-theme"
	fi
}
