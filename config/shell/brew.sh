# shellcheck shell=sh

# brew
if [ -z "$HOMEBREW_PREFIX" ] || ! [ -e "$HOMEBREW_PREFIX"/bin/brew ]; then
	if [ -e /System ]; then
		# macOS
		if [ -e /opt/homebrew/bin/brew ]; then export HOMEBREW_PREFIX=/opt/homebrew
		elif [ -e /usr/local/bin/brew ]; then export HOMEBREW_PREFIX=/usr/local
		fi
	else
		if [ -e /home/linuxbrew/.linuxbrew/bin/brew ]; then export HOMEBREW_PREFIX=/home/linuxbrew/.linuxbrew
		elif [ -e "$HOME"/.linuxbrew/bin/brew ]; then export HOMEBREW_PREFIX="$HOME"/.linuxbrew
		fi
	fi
fi

if [ -x "$HOMEBREW_PREFIX"/bin/brew ]; then
	# eval "$("$HOMEBREW_PREFIX"/bin/brew shellenv)"
	#OR:
	export HOMEBREW_CELLAR="$HOMEBREW_PREFIX"/Cellar
	if [ -e "$HOMEBREW_PREFIX"/Homebrew/Library ]; then
		export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX"/Homebrew
	else
		export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX"
	fi
	if [ -d "$HOMEBREW_PREFIX" ]; then
		# export PATH="$HOMEBREW_PREFIX"/bin:"$HOMEBREW_PREFIX"/sbin:"$PATH"
		export INFOPATH="${HOMEBREW_PREFIX}/share/info:${INFOPATH:-}";
		[ -z "$ZSH_VERSION" ] || eval 'fpath[1,0]="/opt/homebrew/share/zsh/site-functions"'
	fi

	if [ -e "$HOMEBREW_PREFIX"/opt/llvm/lib/c++ ]; then
		# use homebrew's llvm
		export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib/c++ -L$HOMEBREW_PREFIX/opt/llvm/lib -lunwind"
		export PATH="$HOMEBREW_PREFIX/opt/llvm/bin:$PATH:$HOMEBREW_PREFIX/opt/python/libexec/bin"
		export LDFLAGS="-L$HOMEBREW_PREFIX/opt/llvm/lib"
		export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/llvm/include"
	fi
fi
