#!/usr/bin/env sh

# sh -c "$(curl -fL https://dotfiles.ajai.dev/download.sh)"
# sh -c "$(wget -O - https://dotfiles.ajai.dev/download.sh)"
# TODO: maybe https://stackoverflow.com/a/72601010

set -eu
set -x

confirm() {
	set +x
	printf '%s (y/n) ' "$1"
	read -r REPLY
	case "$REPLY" in
		y*) : ;;
		*) return 2 ;;
	esac
	set -x
}

if ! [ -e "${DOTFILES:=$HOME/prog/dotfiles}" ]; then
	mkdir -p "$(dirname "$DOTFILES")"
	cd "$(dirname "$DOTFILES")"
	echo "Installing to $DOTFILES..."
	if command -v git >/dev/null 2>&1; then
		git clone --depth=1 "https://github.com/AjaiKN/dotfiles"
	else
		confirm "There's no git command in PATH. Use wget/curl instead?" || exit 2

		if command -v wget >/dev/null 2>&1; then
			wget -O dotfiles.tar.gz 'https://api.github.com/repos/AjaiKN/dotfiles/tarball/'
		elif command -v curl >/dev/null 2>&1; then
			curl -Lo dotfiles.tar.gz 'https://api.github.com/repos/AjaiKN/dotfiles/tarball/'
		else
			echo "no curl or wget command available in PATH"
			exit 2
		fi

		mkdir -p "$DOTFILES"
		if command -v tar >/dev/null 2>&1; then
			tar -x -f dotfiles.tar.gz -k -C "$DOTFILES" --strip-components=1
		elif command -v bsdtar >/dev/null 2>&1; then
			bsdtar -x -f dotfiles.tar.gz -k -C "$DOTFILES" --strip-components=1
		else
			echo "no tar command available in PATH"
			exit 2
		fi
	fi
fi

if [ -e "$DOTFILES"/install.sh ]; then
	confirm "Dotfiles are downloaded to $DOTFILES. Install to home directory now?" || exit 0
	exec "$DOTFILES"/install.sh
else
	echo "Failed to download"
	exit 1
fi
