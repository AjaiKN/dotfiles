## .zshrc
# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
# Loaded in all interactive shells in zsh. It
# should contain commands to set up aliases, functions,
# options, key bindings, etc.
#

### Profiling prologue
# SHOULD_PROFILE=yup # uncomment this line to profile zsh startup time
[ -z "$SHOULD_PROFILE" ] || zmodload zsh/zprof

### helpers
echo_if_interactive() {
	if [ -t 0 ]; then
		echo "$@"
	fi
}

zmodload -F zsh/stat b:zstat                   || return 1
zmodload -F zsh/files b:{zf_rm,zf_mv,zf_mkdir} || return 1
zmodload zsh/datetime                          || return 1
function zsh_compile {
	# copied from zsh4humans -z4h-compile (license: MIT)

	# Runs with user options.
	#
	# Precondition: [[ -e $1 ]].

	local file="$1"

	# The trick: whenever we compile, set mtime($file.zwc) <- 1 + mtime($file).
	# We always recompile unless mtime($file.zwc) == 1 + mtime($file).
	# This is more robust than just checking if mtime($file.zwc) >= mtime($file), and it's fast.

	local -a stat

	# Checking [[ -e "$file".zwc ]] is faster than redirecting stderr of zstat to /dev/null.
	[[ -e "$file".zwc ]] &&
		# Use zstat to get mtime of the regular $file and the zwc $file.
		zstat +mtime -A stat -- "$file" "$file".zwc &&
		{
			# Negative indices to handle ksh_arrays:
			#   stat[1] = stat[-2] = mtime(${file})
			#   stat[2] = stat[-1] = mtime(${file}.zwc)
			# If mtime($file.zwc) == 1 + mtime($file), we can return.
			(( stat[-1] == stat[-2] + 1 )) && return
			# If we reach here, that means we'll need to recompile.
			# Remove the second element, mtime(${file}.zwc), from the array.
			stat[-1]=()
			# Now $stat has just one value and can basically be treated like a scalar.
		} || {
			# If we reach here, $file.zwc doesn't exist.
			zstat +mtime -A stat -- "$file" || return
		}
	# $stat = mtime($file)

	# Check that the directory is writable
	[[ -w "${1:h}" ]] || return

	# t = formatted($stat) = formatted(1 + mtime($file))
	local t
	builtin strftime -s t '%Y%m%d%H%M.%S' $((stat + 1))

	# Do the compiling. We first compile it to a tmp file so we can atomically
	# replace $file.zwc only if the compiling is successful.
	local tmp="$file".tmp."${sysparams[pid]}".zwc
	{
		# This zf_rm is to work around bugs in NTFS and/or WSL. The following code fails there:
		#
		#   touch a b
		#   chmod -w b
		#   zf_rm -f a b
		#
		# The last command produces this error:
		#
		#   zf_mv: a: permission denied
		(( !_akn_dangerous_root ))                   &&
			builtin zcompile -R -- "$tmp" "$file"      &&
			command touch -ct $t -- "$tmp"             &&
			zf_rm -fs -- "$file".zwc                   &&
			zf_mv -f -- "$tmp" "$file".zwc
	} always {
		# If it's unsuccessful, delete the tmp file.
		(( $? )) && zf_rm -fs -- "$tmp" "$file".zwc 2>/dev/null
	}
}

function zsh_compile_many {
	local file
	for file in $@; do
		zsh_compile $file
	done
}

function zsh_compile_if_zwc_exists {
	if [[ -e "$1".zwc ]]; then
		zsh_compile "$1"
	fi
}
function safe_source {
	zsh_compile_if_zwc_exists "$1"
	builtin source "$@"
}
function safe_source_dot {
	zsh_compile_if_zwc_exists "$1"
	builtin . "$@"
}
function compile_and_source {
	zsh_compile "$1"
	builtin source "$@"
}
function clean_zwc_files {
	emulate -L zsh -o extended_glob -o no_case_glob -x
	rm -f ~/*.zwc(.ND) ~/*zsh*/**/*.zwc(.ND) ~/.{config,cache}/*zsh*/**/*.zwc(.ND) \
		$DOTFILES/dot-home/*.zwc(.ND) $DOTFILES/config/zsh/**/*.zwc(.ND)
}

alias source=safe_source
alias .=safe_source_dot
function source { safe_source "$@" }
function . { safe_source_dot "$@" }

### Nix prologue
OLD_PATH_ZSHRC=$PATH

### Basic variables

ZSH_CUSTOM="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
ZSH_PLUGINS="$ZSH_CUSTOM/plugins"

if (( _akn_dangerous_root )); then
	if (( $+commands[mktemp] )); then
		ZSH_CACHE_DIR=$(mktemp -d)
	else
		ZSH_CACHE_DIR=/tmp/zsh-cache-dir-$EUID-$RANDOM
		zf_mkdir -m 0700 $ZSH_CACHE_DIR
	fi
fi

### Plugin customization
# speed up zsh-syntax-highlighting in zsh 5.8 and below
# https://github.com/zsh-users/zsh-syntax-highlighting/issues/513
# https://github.com/zsh-users/zsh-syntax-highlighting/issues/295#issuecomment-214581607
autoload -Uz is-at-least
if ! is-at-least 5.9; then
	zstyle ':bracketed-paste-magic' active-widgets '.self-*'
fi

# https://github.com/zsh-users/zsh-autosuggestions#disabling-suggestion-for-large-buffers
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
# https://github.com/zsh-users/zsh-autosuggestions?tab=readme-ov-file#suggestion-strategy
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# for vi-mode plugin
VI_MODE_SET_CURSOR=true

zstyle ':zim:input' double-dot-expand yes

### plugins

compile_and_source "$ZSH_CUSTOM"/plugin-manager.zsh

[[ "${OSTYPE}" == darwin* ]] && plugin scriptingosx/mac-zsh-completions
plugin clarketm/zsh-completions
(( $+commands[zig] )) && plugin ziglang/shell-completions
plugin prezto-completion
plugin tabtab
plugin zsh-functions
# plugin zimfw/completion # calls compinit
is-at-least 4.3 && plugin zsh-users/zsh-history-substring-search
plugin $DOTFILES/vendor/zap
plugin omz-clipboard
plugin omz-functions
plugin omz-grep
plugin omz-history
plugin omz-key-bindings
# disable in mistty: https://mistty.readthedocs.io/en/latest/shells.html#id11
if [ "$TERM" != "eterm-color" ]; then
	plugin ohmyzsh/plugins/vi-mode
	is-at-least 5.2 && plugin zsh-vi-more/vi-motions
fi
plugin zimfw/input
# plugin zimfw/archive # unalias archive unarchive lsarchive 2>/dev/null
(( $+commands[direnv] )) && plugin zimfw/direnv
(( $+commands[mise] )) && plugin mise
plugin omz-termsupport
plugin omz-vcs_info
plugin ohmyzsh/plugins/fasd
[[ "${OSTYPE}" == darwin* ]] && plugin ohmyzsh/plugins/macos
(( $+commands[systemctl] )) && plugin ohmyzsh/plugins/systemd
plugin ohmyzsh/plugins/gitignore
# plugin ohmyzsh/plugins/colored-man-pages
# plugin ohmyzsh/plugins/mise
# plugin ohmyzsh/plugins/gnu-utils
# plugin prezto/modules/gnu-utility
(( $+commands[npm] )) && plugin lukechilds/zsh-better-npm-completion
plugin help
plugin ~/.config/shell/rc.sh --no-compile
plugin options
plugin aliases
plugin compdef
[ "$TERM_PROGRAM" = "iTerm.app" ] && plugin iterm
# plugin google-cloud-sdk
# plugin ocaml
# plugin conda
[[ -e /nix ]] && plugin nix
# plugin direnv
(( $+commands[fzf] )) && plugin fzf
plugin history
plugin edit-command-line
[[ -n $INSIDE_EMACS ]] && plugin inside-emacs
plugin autocorrect
(( $+commands[atuin] )) && plugin atuin

if [[ $TERM != dumb && ( -z $INSIDE_EMACS || $INSIDE_EMACS = vterm* || $TERM = eterm-color ) ]]; then
	is-at-least 4.3.11 && plugin zsh-users/zsh-syntax-highlighting
	ZSH_AUTOSUGGEST_MANUAL_REBIND=1
	# this should be the very last plugin, since I'm setting ZSH_AUTOSUGGEST_MANUAL_REBIND=1
	is-at-least 5.0.8 && plugin zsh-users/zsh-autosuggestions
fi

plugin bindings

load_plugins

### Plugin customization (after)

ZSH_HIGHLIGHT_HIGHLIGHTERS+=(brackets)

### Misc

# Sets color variable such as $fg, $bg, $color and $reset_color
# autoload -U colors && colors

### ~p = ~/prog, ~d = ~/prog/dotfiles
# https://stackoverflow.com/a/28732211
hash -d p=$HOME/prog
hash -d d=$DOTFILES

### KEYTIMEOUT
# Otherwise, there's a delay after pressing ESC before moving to normal mode.
KEYTIMEOUT=1 # 0.01 seconds

### Powerlevel10k
if [[ $ZSH_THEME == "powerlevel10k/powerlevel10k" ]]; then
	# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
	[[ ! -f ~/.p10k.zsh ]] || compile_and_source ~/.p10k.zsh
	[[ ! -f ~/.p10k.mise.zsh ]] || compile_and_source ~/.p10k.mise.zsh
fi

### Emacs  tramp
# https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

### set completion colors to be the same as `ls`, after theme has been loaded
# from oh-my-zsh
[[ -z "$LS_COLORS" ]] || zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

### Nix thing
[ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ] && . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

### make cursor less conspicuous while prompt is being drawn

if [[ $TERM != "dumb" ]]; then
	function →set_cursor_before_prompt {
		echo -ne '\e[6 q' # bar cursor
		echo -ne '\e[?25l' # invisible cursor
	}
	function →set_cursor_after_prompt {
		echo -ne '\e[?25h' # visible cursor
	}
	typeset -ga precmd_functions
	precmd_functions=(→set_cursor_before_prompt $precmd_functions →set_cursor_after_prompt)
fi

### Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_ZSHRC:$PATH

	# Add the value of IN_NIX_SHELL to the beginning of the prompt.
	# PROMPT="[IN_NIX_SHELL=$IN_NIX_SHELL] $PROMPT"
fi

### Secure PATH

if [ -x "$DOTFILES/scripts/secure_path" ]; then
	if [ -t 0 ]; then
		PATH="$("$DOTFILES/scripts/secure_path" || printf '%s' "$PATH")"
	else
		# if not interactive, don't let it print anything
		PATH="$("$DOTFILES/scripts/secure_path" 2>/dev/null || printf '%s' "$PATH")"
	fi
	export PATH
fi

### Deduplicate PATH, MANPATH, and fpath
typeset -U path manpath fpath

### Profiling epilogue
[ -z "$SHOULD_PROFILE" ] || zprof
