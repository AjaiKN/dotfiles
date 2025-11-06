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

function .set_zsh_plugin_default_branch {
	local default_branch=$(curl -L -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" "https://api.github.com/repos/${1}" | jq -r .default_branch)
	if [ -n "$default_branch" ] && [ "$default_branch" != null ]; then
		if (( _akn_dangerous_root )); then
			>&2 echo "Refusing because root"
			return 1
		fi
		(
			set -x
			git -C "$DOTFILES" submodule set-branch -b "$default_branch" -- "config/zsh/plugins/${1}"
		)
	fi
}

function .zsh_plugin__is_empty_dir {
	[[ -d "$1" ]] || return 2
	for file in "$1"/*(N); do
		return 1
	done
	return 0
}

function .zsh_plugin__is_nonempty_dir {
	[[ -d "$1" ]] || return 2
	! .zsh_plugin__is_empty_dir "$@"
}

function install_zsh_plugin {
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing because root"
		return 1
	fi
	init_zsh_plugin "$@" || {
		local url="https://github.com/${1}"
		(
			set -x
			mkdir -p "$DOTFILES/config/zsh/plugins/${1:h}"
			git -C "$DOTFILES" submodule add --depth=1 "$url" "config/zsh/plugins/${1}"
		)
		.set_zsh_plugin_default_branch "$1"
	}
}

function uninstall_zsh_plugin {
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing because root"
		return 1
	fi
	(
		set -x
		git -C "$DOTFILES" rm -f "config/zsh/plugins/${1}"
	)
}

function update_zsh_plugin {
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing because root"
		return 1
	fi
	.set_zsh_plugin_default_branch "$1"
	(
		set -x
		git -C "$DOTFILES" submodule update --remote --depth=1 "config/zsh/plugins/${1}"
	)
}

function init_zsh_plugin {
	if (( _akn_dangerous_root )); then
		>&2 echo "Refusing because root"
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
	(( $# == 0 )) && return 1
	for dir in "$@"; do
		if [[ -d "$dir" ]]; then
			fpath=("$dir" $fpath)
		fi
	done
}
function add_to_path {
	(( $# == 0 )) && return 1
	for dir in "$@"; do
		if [[ -d "$dir" ]]; then
			path=("$dir" $path)
	fi
	done
}

function autoload_from {
	builtin emulate -L zsh -o EXTENDED_GLOB
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
	local possible_paths plugin_file
	local plugin=$1
	possible_paths=(
		# ":t" gets the basename, in case the plugin name includes the full repo name
		"$ZSH_PLUGINS/${plugin}/${plugin:t}.plugin.zsh"(.N) # regular plugins, ohmyzsh plugins
		"$ZSH_PLUGINS/${plugin}/init.zsh"(.N) # prezto modules, zim modules
		"$ZSH_PLUGINS/${plugin}.zsh"(.N)
		"$ZSH_PLUGINS/${plugin}.plugin.zsh"(.N)
		"$ZSH_PLUGINS/${plugin}"/*.plugin.zsh(.N)
		"$ZSH_PLUGINS/${plugin}"/*.zsh(.N)
	)
	if [ ${#possible_paths[@]} -eq 0 ]; then
		if [[ -o interactive ]]; then
			if [ -z $retrying_after_submodule_init ] && init_zsh_plugin $plugin; then
				retrying_after_submodule_init=1 load_plugin "$@"
				return $?
			else
				>&2 echo "Plugin not found: ${plugin}"
				>&2 echo "To add it as a submodule, run:"
				>&2 echo "install_zsh_plugin ${plugin}"
				>&2 echo
				return 1
			fi
		else
			return 1
		fi
	else
		zsh_loaded_plugins+=( "${plugin:t}" )
		plugin_file="${possible_paths[@]:0:1}"
		typeset -F start_time=EPOCHREALTIME
		# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#zero-handling
		ZERO="$plugin_file" compile_and_source "$plugin_file"
		typeset -F end_time=EPOCHREALTIME
		plugin_times[$plugin]=$((end_time - start_time))
		return
	fi
}

# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#indicator
typeset -ga zsh_loaded_plugins
typeset -gA plugin_times

typeset -gaU plugins_failed
typeset -gaU plugins

function print_plugin_times {
	(
		for plugin in $plugins; do
			printf "%'9dμs %s\n" $(( 1000000 * ${plugin_times[$plugin]} )) $plugin
		done
	) | sort -n --reverse
}

function plugin {
	plugins+=("$1")
}

function load_plugins {
	# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html#pmspec
	export PMSPEC=0fbis

	unset -f plugin

	if (( ! _akn_dangerous_root )); then
		mkdir -p -m 0700 "$ZSH_CACHE_DIR/completions"
	fi

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

	add_to_fpath "$ZSH_CACHE_DIR/completions" "$ZSH_CUSTOM/completions" "$ZSH_CUSTOM/functions"
	autoload_from "$ZSH_CUSTOM/functions"
	for plugin in $plugins; do
		# zim modules and prezto modules expect their functions to be autoloaded
		autoload_from "$ZSH_PLUGINS/${plugin:t}/functions"(/N)
	done

	# actually load the plugins
	for plugin in $plugins; do
		load_plugin "$plugin" || {
			# If it failed to load, remove it from plugins array and add it to plugins_failed
			plugins[$plugins[(i)$plugin]]=()
			plugins_failed+=("$plugin")
		}
	done
	unset plugin

	# load the theme
	if [[ -n "$ZSH_THEME" ]]; then
		compile_and_source "$ZSH_CUSTOM/themes/$ZSH_THEME.zsh-theme"
	fi

	akn-setup-compinit
}

# from zsh4humans: defer compdef calls for later
function compdef() {
	emulate -L zsh &&
		setopt typeset_silent pipe_fail extended_glob prompt_percent no_prompt_subst &&
		setopt no_prompt_bang no_bg_nice no_aliases
	_akn_compdef+=("${(pj:\0:)@}")
}
