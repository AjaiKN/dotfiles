# shellcheck shell=sh
# shellcheck disable=SC2317

# .shell_env.sh - Setting basic environment variables
# Loaded by bash, zsh, and sh shells.
#   - ~/.zshenv (zsh)
#   - ~/.bashrc (bash)
#   - ~/.profile (sh and others)

export ENV="$HOME/.shell_env.sh"
export BASH_ENV="$HOME/.shell_env.sh"

## umask
umask o-w

## Nix prologue
OLD_PATH_SHELL_ENV=$PATH

## DOTFILES

if [ -L ~/.zshenv ] && command -v realpath >/dev/null 2>&1; then
	DOTFILES="$(dirname -- "$(dirname -- "$(realpath ~/.zshenv)")")"
elif [ -L ~/.zshenv ] && command -v perl >/dev/null 2>&1; then
	# from comment in https://unix.stackexchange.com/questions/720080/how-do-i-resolve-a-relative-path-in-a-posix-shell-if-readlink-realpath-is-not-av
	DOTFILES="$(dirname -- "$(dirname -- "$(perl -MCwd=realpath -le 'print realpath(shift)' ~/.zshenv)")")"
elif [ -L ~/.zshenv ] && command -v readlink >/dev/null 2>&1; then
	case $(readlink ~/.zshenv) in
		/*) # absolute path
			DOTFILES="$(dirname -- "$(dirname -- "$(readlink ~/.zshenv)")")"
			;;
		*) # relative path
			DOTFILES="$(dirname -- "$(dirname -- "$HOME/$(readlink ~/.zshenv)")")"
			;;
	esac
elif [ -d "$HOME/prog/dotfiles" ]; then
	DOTFILES="$HOME/prog/dotfiles"
elif [ -d "$HOME/dotfiles" ]; then
	DOTFILES="$HOME/dotfiles"
elif [ -d "$HOME/.dotfiles" ]; then
	DOTFILES="$HOME/.dotfiles"
else
	[ -t 0 ] && echo "Unable to locate DOTFILES directory" >&2
	DOTFILES="$HOME/prog/dotfiles"
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

export HOMEBREW_PREFIX="/opt/homebrew" # fallback so that HOMEBREW_PREFIX isn't blank
if command -v brew >/dev/null 2>&1; then
	eval "$(brew shellenv)" # this modifies PATH; to see the script, just run the command in the parens
elif ! [ -e /System ] && [ -x /home/linuxbrew/.linuxbrew/bin/brew ]; then
	# NOTE: Don't add anything with /home to PATH on Mac. /home exists on Mac
	# (not sure why) and is symlinked to /System/Volumes/Data/home, which is slow
	# to access for some reason. So if it's on PATH, it'll slow down every
	# command.
	eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# # https://github.com/romkatv/zsh4humans/blob/cd6c4770c802c3a17b4c43e5587adabb9a370a75/main.zsh#L67-L87
# export HOMEBREW_PREFIX="/opt/homebrew" # fallback so that HOMEBREW_PREFIX isn't blank
# if [ -e /System ]; then
# 	if [ -x /opt/homebrew/bin/brew ]; then HOMEBREW_PREFIX=/opt/homebrew
# 	elif [ -x /usr/local/bin/brew ]; then HOMEBREW_PREFIX=/usr/local
# 	fi
# else
# 	if [ -x /home/linuxbrew/.linuxbrew/bin/brew ]; then HOMEBREW_PREFIX=/home/linuxbrew/.linuxbrew
# 	elif [ -x "$HOME"/.linuxbrew/bin/brew ]; then HOMEBREW_PREFIX="$HOME"/.linuxbrew
# 	fi
# fi
# export HOMEBREW_PREFIX
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

if [ -d "$HOMEBREW_PREFIX" ]; then
	# use homebrew's llvm
	export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib/c++ -L$HOMEBREW_PREFIX/opt/llvm/lib -lunwind"
	export PATH="$HOMEBREW_PREFIX/opt/llvm/bin:$PATH:$HOMEBREW_PREFIX/opt/python/libexec/bin"
	export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib"
	export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/llvm/include"
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

export LESS='-R'
export PAGER=less
export GIT_PAGER="less -FX"

# bat is called batcat on Ubuntu & Debian
if command -v bat >/dev/null 2>&1 || command -v batcat >/dev/null 2>&1; then
	export MANPAGER="bat_manpager"
	if command -v delta >/dev/null 2>&1; then
		export GIT_PAGER="delta"
	fi
fi

### PS4 (used by set -x and bash -x)
# $PS4 is the prefix used by `set -x`, and it's '+ ' by default.
# $SHLVL is the current number of nested shells.
#
# If I have a bash script that uses `set -x`, and that calls another bash script
# which uses `set -x`, I want to make it clear that the commands are nested a
# level further. To do this, I insert an extra $SHLVL "+"s into $PS4.
#
# In POSIX sh, PS4 only has to support parameter expansion (env vars ${}), not
# command substitution. For example, dash doesn't support command subsitution
# in PS4. So we check that we're in either bash or zsh.
if [ -n "$BASH_VERSION" ]; then #|| [ -n "$ZSH_VERSION" ]; then
	PS4='$(printf "+%.0s" $(seq ${SHLVL:-1})) '
	# Don't export so it doesn't get inherited by shells like dash.
fi

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_SHELL_ENV:$PATH
fi
