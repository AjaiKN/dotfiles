# shellcheck shell=sh

## lowest priority
export PLAN9="$HOME/prog/plan9port"

export PATH="$PATH:\
$HOME/.antigravity/antigravity/bin:\
/opt/R/arm64/gfortran/bin:\
/Applications/Firefox.app/Contents/MacOS:\
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:\
$PLAN9/bin\
"

## system = low-mid priority

## mid priority
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
$PATH"

## brew (mid-high priority)
if [ -z "$HOMEBREW_PREFIX" ] || ! [ -e "$HOMEBREW_PREFIX"/bin/brew ]; then
	if [ -e /System ]; then
		# macOS
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

## highest priority

export PATH="\
$HOME/.local/bin:\
$HOME/bin:\
$DOTFILES/private/bin:$DOTFILES/bin:\
$HOME/.local/share/mise/shims:\
$HOME/.config/emacs/bin:\
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

## Secure PATH
if [ -x "$DOTFILES/scripts/secure_path" ]; then
	if [ -t 0 ]; then
		PATH="$("$DOTFILES/scripts/secure_path" || printf '%s' "$PATH")"
	else
		# if not interactive, don't let it print anything
		PATH="$("$DOTFILES/scripts/secure_path" 2>/dev/null || printf '%s' "$PATH")"
	fi
	export PATH
fi
