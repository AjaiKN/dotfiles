# Ajai's Zsh Plugin Manager
#
# DESCRIPTION:
#   A git submodule-based plugin manager for Zsh
#
# FEATURES:
#   - Follows the Zsh Plugin Standard: https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html
#   - Git submodule-based: Plugins are git submodules in my dotfiles repository
#   - Automatic initialization: Missing plugins are initialized on first use
#   - Performance tracking: Built-in timing for plugin load performance
#   - Path management: Automatic fpath and PATH setup
#
# PLUGIN STORAGE:
#   Plugins are stored in: $DOTFILES/config/zsh/plugins/USERNAME/REPO-NAME
#
# SUPPORTED PLUGIN FORMATS:
#   - Standard zsh plugins: *.plugin.zsh files (https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html)
#   - Oh My Zsh plugins: *.plugin.zsh files
#   - Prezto modules: init.zsh files
#   - Zim modules: init.zsh files
#   - One-file plugins: Any .zsh or .plugin.zsh file in the plugin directory
#
# MAIN FUNCTIONS:
#
#   install_zsh_plugin USERNAME/REPO-NAME
#     Install a new plugin from GitHub as a git submodule
#
#   update_zsh_plugin USERNAME/REPO-NAME
#     Update an existing plugin to latest version
#
#   uninstall_zsh_plugin USERNAME/REPO-NAME
#     Remove a plugin and its submodule
#
#   plugin USERNAME/REPO-NAME
#   plugin PLUGIN-NAME
#   plugin ohmyzsh/plugins/OMZ-PLUGIN-NAME
#     Queue a plugin for loading (used in .zshrc)
#
#   load_plugins
#     Load all queued plugins (call this after all plugin() calls)
#
#   print_plugin_times
#     Show plugin load times for performance debugging
#
# USAGE IN .ZSHRC:
#   source "$DOTFILES/config/zsh/plugin-manager.zsh"
#
#   ZSH_THEME="simple"
#
#   plugin ohmyzsh/plugins/vi-mode
#   plugin zsh-users/zsh-syntax-highlighting
#   plugin zsh-users/zsh-autosuggestions
#
#   load_plugins
#
# EXAMPLES:
#   # Install a new plugin
#   install_zsh_plugin zsh-users/zsh-completions
#
#   # Update all plugins
#   cd $DOTFILES && git submodule update --remote
#
#   # Debug slow startup
#   print_plugin_times
#
#   # Remove unused plugin
#   uninstall_zsh_plugin old/unused-plugin
#
# ENVIRONMENT VARIABLES:
#   ZSH_PLUGINS     - Directory containing plugin submodules
#   ZSH_CACHE_DIR   - Cache directory for completions
#   ZSH_CUSTOM      - Custom zsh configuration directory
#   DOTFILES        - Root dotfiles directory
#
# ARRAYS POPULATED:
#   zsh_loaded_plugins  - List of successfully loaded plugin names
#   plugin_times        - Load times for each plugin (microseconds)
#   plugins_failed      - List of plugins that failed to load
#   plugins             - List of plugins to load

builtin zmodload -F zsh/files b:zf_mkdir || {
	function zf_mkdir { mkdir $@ }
}

function .set_zsh_plugin_default_branch {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	local default_branch=$(curl -L -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" "https://api.github.com/repos/${1}" | jq -r .default_branch)
	if [ -n "$default_branch" ] && [ "$default_branch" != null ]; then
		if (( _akn_dangerous_root )); then
			>&2 echo "Refusing because root"
			return 1
		fi
		(
			set -x
			git -C "$DOTFILES" submodule set-branch -b "$default_branch" -- "config/zsh/plugins/${1}"
			git -C "$DOTFILES" add .gitmodules
		)
	fi
}

function .zsh_plugin__is_empty_dir {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	[[ -d "$1" ]] || return 2
	for file in "$1"/*(N); do
		return 1
	done
	return 0
}

function .zsh_plugin__is_nonempty_dir {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	[[ -d "$1" ]] || return 2
	! .zsh_plugin__is_empty_dir "$@"
}

function install_zsh_plugin {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing to init zsh plugin $1 because root"
		return 1
	fi
	init_zsh_plugin "$@" || {
		local url="https://github.com/${1}"
		(
			set -x
			zf_mkdir -p "$DOTFILES/config/zsh/plugins/${1:h}"
			git -C "$DOTFILES" submodule add --depth=1 "$url" "config/zsh/plugins/${1}"
		)
		.set_zsh_plugin_default_branch "$1"
	}
}

function uninstall_zsh_plugin {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing to init zsh plugin $1 because root"
		return 1
	fi
	(
		set -x
		git -C "$DOTFILES" rm -f "config/zsh/plugins/${1}"
	)
}

function update_zsh_plugin {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing to init zsh plugin $1 because root"
		return 1
	fi
	.set_zsh_plugin_default_branch "$1"
	(
		set -x
		git -C "$DOTFILES" submodule update --remote --depth=1 "config/zsh/plugins/${1}"
	)
}

function init_zsh_plugin {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing to init zsh plugin $1 because root"
		return 1
	fi
	if [[ $1 == ohmyzsh/* ]]; then
		init_zsh_plugin ohmyzsh
		return $?
	fi
	[[ -o interactive ]] || return 5
	(( $+commands[git] )) || return 2
	.zsh_plugin__is_empty_dir "$DOTFILES/config/zsh/plugins/$1" || return 3
	(
		cd "$DOTFILES" || return 1
		>&2 echo "Initializing plugin submodule $1"
		set -x
		git submodule update --init --depth=1 --recursive "config/zsh/plugins/$1"
	) || return 4
	>&2 echo
	.zsh_plugin__is_nonempty_dir "$DOTFILES/config/zsh/plugins/$1"
}

function add_to_fpath {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	(( $# == 0 )) && return 1
	for dir in "$@"; do
		if [[ -d "$dir" ]]; then
			fpath=("$dir" $fpath)
		fi
	done
}
function add_to_path {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	(( $# == 0 )) && return 1
	for dir in "$@"; do
		if [[ -d "$dir" ]]; then
			path=("$dir" $path)
	fi
	done
}

function autoload_from {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	for dir in "$@"; do
		if [ -d "$dir" ]; then
			local function_glob='^([_.]*|prompt_*_setup|README*|*~)(-.N:t)' # from prezto
			for func in "${dir}"/$~function_glob; do
				autoload -Uz "$func"
			done
		fi
	done
}

function load_plugin {
	# Don't do `emulate -L zsh` here! Or else plugins won't be able to set options.

	local plugin=$1
	zsh_loaded_plugins+=($plugin_shortnames[$plugin])
	local plugin_file=${plugin_files[$plugin]:-}
	if [[ -n $plugin_file ]]; then
		typeset -F start_time=EPOCHREALTIME
		# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#zero-handling
		ZERO="$plugin_file" compile_and_source "$plugin_file"
		local ret=$?
		typeset -F end_time=EPOCHREALTIME
		plugin_times[$plugin]=$((end_time - start_time))
		return $ret
	fi
	return
}

# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#indicator
typeset -ga zsh_loaded_plugins
typeset -gA plugin_files
typeset -gA plugin_types
typeset -gA plugin_shortnames
typeset -gA plugin_dirs
typeset -gA plugin_times

typeset -gaU plugins_failed
typeset -gaU plugins

function plugin_info {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	printf "%'11s %-45s %-20s %-20s\n" "TIME" "ID" "TYPE" "SHORTNAME"
	(
		for plugin in $plugins; do
			printf "%'9dμs %-45s %-20s %-20s\n" $(( 1000000 * ${plugin_times[$plugin]:-0} )) $plugin $plugin_types[$plugin] $plugin_shortnames[$plugin]
		done
	) | sort -n --reverse
}

function .plugin_file {
	builtin emulate -L zsh -o extended_glob -o no_case_glob -o no_aliases
	local plugin=$1; shift
	local plugin_type=$1; shift
	if (( $# > 0 )); then
		local plugin_filename=$1
		plugins+=($plugin)
		plugin_types[$plugin]=$plugin_type
		if [[ $plugin_type == dir_only ]]; then
			plugin_shortnames[$plugin]=${plugin:t}
			plugin_dirs[$plugin]=${plugin_filename}
			return
		fi
		plugin_files[$plugin]=$plugin_filename
		if [[ $plugin_type == regular ]]; then
			plugin_shortnames[$plugin]=${${plugin_filename:t}%.plugin.zsh}
		else
			plugin_shortnames[$plugin]=${plugin:t}
		fi
		if [[ $plugin_type != single_file ]]; then
			plugin_dirs[$plugin]=${plugin_filename:h}
		fi
	else
		return 2
	fi
}

# probably eventually want commands similar to https://zimfw.sh/docs/commands/
# TODO:
# - full git URLs
# - eval/cmd (maybe with optional cache)
function plugin {
	builtin emulate -L zsh -o EXTENDED_GLOB -o NO_CASE_GLOB
	local plugin=$1
	local plugin_basic_path=$ZSH_PLUGINS/${plugin}
	if [[ $plugin == /* || $plugin = '~'* ]]; then
		plugin_basic_path=$plugin
	fi
	.plugin_file   $plugin regular        $plugin_basic_path/${plugin:t}.plugin.zsh(NY1^/) ||
		.plugin_file $plugin regular        $plugin_basic_path/*.plugin.zsh(NY1^/) ||
		.plugin_file $plugin prezto_or_zim  $plugin_basic_path/init.zsh(NY1^/) ||
		.plugin_file $plugin nonstandard    $plugin_basic_path/*.zsh(NY1^/) ||
		.plugin_file $plugin nonstandard_sh $plugin_basic_path/*.sh(NY1^/) ||
		.plugin_file $plugin single_file    $plugin_basic_path.plugin.zsh(NY1^/) ||
		.plugin_file $plugin single_file    $plugin_basic_path.zsh(NY1^/) ||
		.plugin_file $plugin dir_only       $plugin_basic_path(NY1F) ||
		{
			if [[ -o interactive ]]; then
				if [ -z $retrying_after_submodule_init ] && init_zsh_plugin $plugin; then
					retrying_after_submodule_init=1 plugin "$@"
					return $?
				else
					>&2 echo "Plugin not found: ${plugin}"
					>&2 echo "To add it as a submodule, run:"
					>&2 echo "install_zsh_plugin ${plugin}"
					>&2 echo
				fi
			fi
			plugins_failed+=("$plugin")
			return 1
		}
}

# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#pmspec
PMSPEC=0fbis

function load_plugins {
	# Don't do `emulate -L zsh` here! Or else plugins won't be able to set options.

	unset -f plugin

	if (( ! _akn_dangerous_root )); then
		zf_mkdir -p -m 0700 "$ZSH_CACHE_DIR/completions"
	fi

	add_to_fpath "$ZSH_CACHE_DIR/completions" "$ZSH_CUSTOM/completions" "$ZSH_CUSTOM/functions"
	autoload_from "$ZSH_CUSTOM/functions"

	local plugin

	# setup fpath and path
	for plugin in $plugins; do
		local plugin_dir=${plugin_dirs[$plugin]:-}
		if [[ -n $plugin_dir && -d $plugin_dir ]]; then
			# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#funcs-dir
			if ! add_to_fpath $plugin_dir/functions(/N); then
				add_to_fpath $plugin_dir(/N)           # omz modules
				add_to_fpath $plugin_dir/src(/N)       # zsh-users/zsh-completions
			fi
			# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#bin-dir
			add_to_path $plugin_dir/bin(/N)

			if [[ $plugin_types[$plugin] == prezto_or_zim ]]; then
				# zim and prezto modules expect their functions to be autoloaded
				autoload_from $plugin_dir/functions(/N)
			fi
		fi
	done

	# actually load the plugins
	for plugin in $plugins; do
		load_plugin "$plugin" || {
			# If it failed to load, remove it from plugins array and add it to plugins_failed
			plugins[$plugins[(i)$plugin]]=()
			plugins_failed+=("$plugin")
		}
	done

	# load the theme
	if [[ -n "$ZSH_THEME" ]]; then
		compile_and_source "$ZSH_CUSTOM/themes/$ZSH_THEME.zsh-theme"
	fi

	akn-setup-compinit
}

# from zsh4humans: defer compdef calls for later
function compdef() {
	builtin emulate -L zsh &&
		setopt typeset_silent pipe_fail extended_glob prompt_percent no_prompt_subst &&
		setopt no_prompt_bang no_bg_nice no_aliases
	_akn_compdef+=("${(pj:\0:)@}")
}
