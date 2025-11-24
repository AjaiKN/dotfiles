# shellcheck shell=sh
# shellcheck disable=SC2317

# ~/.config/shell/shared/envvars.sh is loaded by:
# - ~/.config/shell/shared/profile.sh: at login (this is the most important!)
# - ~/.config/shell/shared/rc.sh: in interactive bash and zsh (for convenience in case this file has changed since login)
# - ~/.config/shell/shared/env.sh ONLY IF THIS FILE HASN'T BEEN LOADED ALREADY: whenever zsh starts (for convenience in case dotfiles weren't installed yet when I logged in)

## umask
umask go-rwx

## DOTFILES

if [ -d "$HOME/.dotfiles" ]; then
	DOTFILES="$( (cd "$HOME/.dotfiles" >/dev/null && pwd -P) )"
elif [ -d "$HOME/prog/dotfiles" ]; then
	DOTFILES="$HOME/prog/dotfiles"
elif [ -d "$HOME/dotfiles" ]; then
	DOTFILES="$HOME/dotfiles"
elif [ -d "$HOME/.dotfiles" ]; then
	DOTFILES="$HOME/.dotfiles"
else
	[ -t 0 ] && echo "Unable to locate DOTFILES directory" >&2
	DOTFILES="$HOME/.dotfiles"
fi
export DOTFILES

## XDG Base Directory
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}" # "data that should persist between (application) restarts, but that is not important or portable enough to the user that it should be stored in $XDG_DATA_HOME"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_DIRS="${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
export XDG_CONFIG_DIRS="${XDG_CONFIG_DIRS:-/etc/xdg}"
# XDG_RUNTIME_DIR

## PATH
export PLAN9="$HOME/prog/plan9port"

export PATH="\
/opt/homebrew/bin:\
/snap/bin:\
/usr/local/bin:\
/usr/local/sbin:\
/opt/local/bin:\
/opt/local/sbin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin:\
$PATH:\
/opt/R/arm64/gfortran/bin:\
/Applications/Firefox.app/Contents/MacOS:\
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:\
$PLAN9/bin"

if [ -z "$HOMEBREW_PREFIX" ] || ! [ -e "$HOMEBREW_PREFIX"/bin/brew ]; then
	if [ -e /System ]; then
		if [ -e /opt/homebrew/bin/brew ]; then HOMEBREW_PREFIX=/opt/homebrew
		elif [ -e /usr/local/bin/brew ]; then HOMEBREW_PREFIX=/usr/local
		fi
	else
		if [ -e /home/linuxbrew/.linuxbrew/bin/brew ]; then HOMEBREW_PREFIX=/home/linuxbrew/.linuxbrew
		elif [ -e "$HOME"/.linuxbrew/bin/brew ]; then HOMEBREW_PREFIX="$HOME"/.linuxbrew
		fi
	fi
	export HOMEBREW_PREFIX
fi

if [ -x "$HOMEBREW_PREFIX"/bin/brew ]; then
	eval "$("$HOMEBREW_PREFIX"/bin/brew shellenv)"
	#OR:
	# export HOMEBREW_CELLAR="$HOMEBREW_PREFIX"/Cellar
	# if [ -e "$HOMEBREW_PREFIX"/Homebrew/Library ]; then
	# 	export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX"/Homebrew
	# else
	# 	export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX"
	# fi
	# if [ -d "$HOMEBREW_PREFIX" ]; then
	# 	export PATH="$HOMEBREW_PREFIX"/bin:"$HOMEBREW_PREFIX"/sbin:"$PATH"
	# 	[ -z "${MANPATH-}" ] || export MANPATH=":${MANPATH#:}";
	# 	export INFOPATH="${HOMEBREW_PREFIX}/share/info:${INFOPATH:-}";
	# fi
	# # TODO: zsh fpath

	if [ -e "$HOMEBREW_PREFIX"/opt/llvm/lib/c++ ]; then
		# use homebrew's llvm
		export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib/c++ -L$HOMEBREW_PREFIX/opt/llvm/lib -lunwind"
		export PATH="$HOMEBREW_PREFIX/opt/llvm/bin:$PATH:$HOMEBREW_PREFIX/opt/python/libexec/bin"
		export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib"
		export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/llvm/include"
	fi
fi

export PATH="\
$HOME/.local/bin:\
$HOME/bin:\
$DOTFILES/private/bin:$DOTFILES/bin:\
$HOME/.local/share/mise/shims:\
$HOME/.config/emacs/bin:\
$DOTFILES/brightness:\
/run/wrappers/bin:\
$HOME/.nix-profile/bin:\
/nix/profile/bin:\
$HOME/.local/state/nix/profile/bin:\
/etc/profiles/per-user/$USER/bin:\
/nix/var/nix/profiles/default/bin:\
/run/current-system/sw/bin:\
$HOME/.cargo/bin:\
$HOME/.poetry/bin:\
/Applications/Coq-Platform~8.16~2022.09.app/Contents/Resources/bin:\
$PATH"

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

## EDITOR

# When we're inside Emacs, we want to check if EDITOR, GIT_EDITOR, or VISUAL was
# probably set by Emacs with-editor (https://magit.vc/manual/with-editor.html).
# If so, that's the editor we'll use.
was_set_by_emacs_witheditor() {
	case "$1" in
		*emacsclient*|*WITH-EDITOR*) return 0 ;;
		*) return 1 ;;
	esac
}

my_editor=
if [ -n "$INSIDE_EMACS" ] && was_set_by_emacs_witheditor "$EDITOR"; then
	my_editor="$EDITOR"
elif [ -n "$INSIDE_EMACS" ] && was_set_by_emacs_witheditor "$GIT_EDITOR"; then
	my_editor="$GIT_EDITOR"
elif [ -n "$INSIDE_EMACS" ] && was_set_by_emacs_witheditor "$VISUAL"; then
	my_editor="$VISUAL"
elif [ -n "$INSIDE_EMACS" ] && command -v emacsclient >/dev/null 2>&1; then
	my_editor="emacsclient"
elif [ "$TERM_PROGRAM" = "vscode" ] && command -v code >/dev/null 2>&1; then
	my_editor="code --wait"
elif command -v sw_vers >/dev/null 2>&1 && command -v emacsclient >/dev/null 2>&1 && [ -x "$DOTFILES/bin/emacs-term" ]; then
	my_editor="$DOTFILES/bin/emacs-term"
elif command -v nvim >/dev/null 2>&1; then
	my_editor="nvim"
elif command -v vim >/dev/null 2>&1; then
	my_editor="vim"
elif command -v emacs >/dev/null 2>&1; then
	my_editor="emacs"
elif command -v vi >/dev/null 2>&1; then
	my_editor="vi"
elif command -v mg >/dev/null 2>&1; then
	my_editor="mg"
elif command -v nano >/dev/null 2>&1; then
	my_editor="nano"
elif command -v code >/dev/null 2>&1; then
	my_editor="code --wait"
fi

if [ -n "$my_editor" ]; then
	EDITOR="$my_editor"
	export EDITOR

	# VISUAL="$my_editor"
	# export VISUAL
	unset VISUAL

	# GIT_EDITOR="$my_editor"
	# export GIT_EDITOR
	unset GIT_EDITOR
fi
unset my_editor
unset -f was_set_by_emacs_witheditor

## Pagers

# -R: color
# -i: case-insensitive search unless search string contains uppercase letters
# -x4: tabs are 4 instead of 8
# -z-4: default scrolling window size = screen height - 4
export LESS='-R -i -x4 -z-4'
export PAGER=less
export GIT_PAGER="less -FX"

# bat is called batcat on Ubuntu & Debian
if command -v bat >/dev/null 2>&1 || command -v batcat >/dev/null 2>&1; then
	export MANPAGER="bat_manpager"
	if command -v delta >/dev/null 2>&1; then
		export GIT_PAGER="delta"
	fi
fi

## Misc

# https://aquaproj.github.io/docs/tutorial/global-config
export AQUA_GLOBAL_CONFIG="${AQUA_GLOBAL_CONFIG:-}:${XDG_CONFIG_HOME:-$HOME/.config}/aquaproj-aqua/aqua.yaml"

## AKN_SHELL_ENVVARS_LOADED
export AKN_SHELL_ENVVARS_LOADED=1
