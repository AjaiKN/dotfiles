## .zshrc

# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
# Loaded in all interactive shells in zsh.

### helpers
echo_if_interactive() {
	if [ -t 0 ]; then
		echo "$@"
	fi
}

function zsh_compile {
	# if $1.zwc doesn't exist or is outdated
	if [[ ! "$1".zwc -nt "$1" ]]; then
		zcompile -R -- "$1"
	fi
}
function zsh_compile_if_zwc_exists {
	# if $1.zwc exists and is outdated
	if [[ "$1".zwc -ot "$1" ]]; then
		zcompile -R -- "$1"
	fi
}
function safe_source {
	zsh_compile_if_zwc_exists "$1"
	builtin source "$1"
}
function compile_and_source {
	zsh_compile "$1"
	builtin source "$1"
}
function clean_zwc_files {
	set -x
	rm -f ~/*.zwc(.ND) ~/*zsh*/**/*.zwc(.ND) ~/.{config,cache}/*zsh*/**/*.zwc(.ND)
	set +x
}

alias source=safe_source
alias .=safe_source
function source { safe_source "$@" }
function . { safe_source "$@" }

### Profiling prologue
# SHOULD_PROFILE=yup # uncomment this line to profile zsh startup time
[ -z "$SHOULD_PROFILE" ] || zmodload zsh/zprof

### Nix prologue
OLD_PATH_ZSHRC=$PATH

### Cursor style
# immediately change cursor style so that it looks like insert mode when the Powerlevel10k instant prompt starts
# (6 = non-blinking bar cursor)
echo_if_interactive -ne '\e[6 q'

### Basic variables

ZSH_CUSTOM="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
ZSH_PLUGINS="$ZSH_CUSTOM/plugins"

### choosing a theme

# version at least 5.1
if ! [[ $ZSH_VERSION != (5.<1->*|<6->.*) ]] && ! [[ -n "$RSTUDIO" ]] && { ! [[ -n "$INSIDE_EMACS" ]] || [[ "$INSIDE_EMACS" == "vterm"* ]] } && { ! [[ "$TERM" = "dumb" ]] }; then
	ZSH_THEME="powerlevel10k/powerlevel10k"
else
	ZSH_THEME="simple"
fi

### Powerlevel10k instant prompt
if [[ $ZSH_THEME == "powerlevel10k/powerlevel10k" ]]; then
	# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
	# Initialization code that may require console input (password prompts, [y/n]
	# confirmations, etc.) must go above this block; everything else may go below.
	if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
		safe_source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
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

# plugin zsh-users/zsh-completions
plugin scriptingosx/mac-zsh-completions
plugin clarketm/zsh-completions
plugin ziglang/shell-completions
plugin prezto-completion
plugin setup-compinit
plugin tabtab
# plugin zimfw/completion # calls compinit
is-at-least 4.3 && plugin zsh-users/zsh-history-substring-search
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
plugin zimfw/direnv
rm -f $ZSH_PLUGINS/joke/zim-mise/mise-activate.zsh{,.zwc} # because PATH might have changed
plugin joke/zim-mise-modified
# plugin mise
plugin omz-termsupport
plugin omz-vcs_info
plugin setup-autoload
plugin ohmyzsh/plugins/fasd
plugin ohmyzsh/plugins/macos
# plugin ohmyzsh/plugins/mise
plugin lukechilds/zsh-better-npm-completion
plugin help
plugin bash_zsh_shared_rc
plugin options
plugin aliases
plugin compdef
plugin iterm
# plugin google-cloud-sdk
plugin ocaml
# plugin conda
plugin nix
# plugin direnv
(( $+commands[fzf] )) && plugin fzf
plugin history
plugin bindings
plugin edit-command-line
plugin emacs
plugin autocorrect
(( $+commands[atuin] )) && plugin atuin

if ! [[ -n "$INSIDE_EMACS" ]] || [[ "$INSIDE_EMACS" = "vterm"* ]] || [[ "$TERM" = "eterm-color" ]] && { ! [[ "$TERM" = "dumb" ]] }; then
	is-at-least 4.3.11 && plugin zsh-users/zsh-syntax-highlighting
	ZSH_AUTOSUGGEST_MANUAL_REBIND=1
	# this should be the very last plugin, since I'm setting ZSH_AUTOSUGGEST_MANUAL_REBIND=1
	is-at-least 5.0.8 && plugin zsh-users/zsh-autosuggestions
fi

load_plugins

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
