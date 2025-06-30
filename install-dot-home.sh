#!/bin/sh

set -eu

DOTFILES=$(realpath "$(dirname "$0")")
export DOTFILES

cd "$DOTFILES"

dotfiles_relative_to_dir() {
	# NOTE: dir must exist
	[ -d "$1" ] || exit 10
	dir=$(realpath "$1")
	res=""
	real_home=$(realpath "$HOME")
	while true; do
		if [ "$dir" = "/" ]; then
			printf "%s" "${res}${DOTFILES}"
			return
		elif [ "$dir" = "$real_home" ] || [ "$dir" = "$HOME" ]; then
			trimmed=$(echo "$DOTFILES" | sed "s|^$real_home/||" | sed "s|^$HOME/||")
			printf "%s" "${res}${trimmed}"
			return
		else
			res="../$res"
			dir=$(dirname "$dir")
		fi
	done
}

get_planned_link_source() {
	base=$(dotfiles_relative_to_dir "$(dirname "$1")")
	trimmed=$(echo "$2" | sed "s|^$DOTFILES/||")
	printf "%s/%s" "$base" "$trimmed"
}

is_dotfiles_link() {
	[ -L "$1" ] || return 1
	relative=$(dotfiles_relative_to_dir "$(dirname "$1")")
	link=$(readlink "$1")
	case "$link" in
		"$DOTFILES"/*|"$relative"/*|./"$relative"/*) return 0 ;;
		*) return 1 ;;
	esac
}

is_self_link() {
	[ -L "$1" ] || return 1
	link=$(readlink "$1")
	base=$(basename "$1")
	[ "$link" = "$base" ] || [ "$link" = "$1" ]
}

make_backup() {
	if [ -e "$1" ]; then
		make_backup "$1.bak" # If $1.bak already exists, make $1.bak.bak
		my_echo "Renaming $1 to $1.bak"
		mv -i "$1" "$1.bak"
	fi
}

is_ignored() {
	base=$(basename "$1")
	case "$base" in
		.DS_Store|.unfold|.#*|*~|'#'*'#') return 0 ;;
		*) return 1 ;;
	esac
}

is_unfold() {
	[ -e "$1/.unfold" ]
}

indent=''
my_echo() {
	if [ $# -eq 0 ]; then
		echo
	else
		first="${indent}$1"
		shift
		echo "$first" "$@"
	fi
}

red=$(printf '\033[31m')
green=$(printf '\033[32m')
# yellow=$(printf '\033[33m')
blue=$(printf '\033[34m')
# bold=$(printf '\033[1m')
reset=$(printf '\033[0m')

handle_file() {
	if [ -L "$1" ]; then
		if is_dotfiles_link "$1" || is_self_link "$1"; then
			if is_ignored "$1"; then
				my_echo "${red}UNLINK (because ignored): $1 -> $(readlink "$1")${reset}"
				rm "$1"
			elif [ -e "$2" ] && [ "$(readlink "$1")" = "$(get_planned_link_source "$1" "$2")" ] && ! is_unfold "$2"; then
				my_echo "ALREADY LINKED: $1 -> $(readlink "$1")"
				return
			else
				my_echo "${red}UNLINK: $1 -> $(readlink "$1")${reset}"
				rm "$1"
			fi
		else
			my_echo "${red}TRASH:  $1 -> $(readlink "$1")${reset}"
			"$DOTFILES"/bin/trash "$1"
		fi
	fi

	if is_ignored "$1"; then
		my_echo "IGNORING: $2"
		return
	fi

	if is_unfold "$2"; then
		mkdir -p "$1"
		my_echo "${blue}UNFOLDING: $1/ -> $(dotfiles_relative_to_dir "$1")/$(echo "$2" | sed "s|^$DOTFILES/||")/${reset}"
		(
			indent="  $indent"

			cd "$2" || exit 50
			for file in * .*; do
				if ! [ "$file" = "." ] && ! [ "$file" = ".." ]; then
					handle_file "$1/$file" "$2/$file"
				fi
			done

			my_echo "Checking for orphaned symlinks in $1..."
			cd "$1" || exit 51
			for file in * .*; do
				if ! [ "$file" = "." ] && ! [ "$file" = ".." ]; then
					if is_dotfiles_link "$1/$file" && ! [ -e "$(readlink "$1/$file")" ]; then
						my_echo "${red}UNLINK (orphan): $1/$file -> $(readlink "$1/$file")${reset}"
						rm "$1/$file"
					fi
				fi
			done
		)
	else
		if [ -e "$1" ]; then
			echo "
$1 should not exist.
(1) Backup $1 to $1.bak
(2) Adopt: move $1 to $(realpath "$2" 2>/dev/null || echo "$2")
(3) Trash $1
(4) do Nothing
(5) Skip $1
(6) Cancel
"
			printf "Enter choice: "
			read -r REPLY
			my_echo

			case $(printf %s "$REPLY" | awk '{print tolower(substr($0,1,1))}') in
				1|b)
					make_backup "$1"
					;;
				2|a)
					"$DOTFILES"/bin/trash -v "$2"
					mv -nv "$1" "$2"
					;;
				3|t)
					"$DOTFILES"/bin/trash -v "$1"
					;;
				4|n)
					;;
				5|s)
					return 0
					;;
				6|c)
					exit 2
					;;
				*)
					my_echo "ERROR: invalid choice: '$REPLY'"
					exit 3
					;;
			esac
		fi

		if [ -e "$2" ]; then
			if [ -e "$1" ]; then
				exit 30
			fi
			my_echo "${green}LINK:   $1 -> $(get_planned_link_source "$1" "$2")${reset}"
			ln -s "$(get_planned_link_source "$1" "$2")" "$1"
		fi
	fi
}

## Pre

cd "$DOTFILES" || exit 54

# nano's backupdir must exist or it gets mad

mkdir -p "$HOME/.cache/nano/backups/"
./submodules-update.sh || :

mkdir -p "$HOME/.config"

## backup dotfiles in home directory
cd "$DOTFILES/dot-home/" || exit 55

### emacs files that shouldn't exist (would override ~/.config/emacs)
# https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html
handle_file "$HOME/.emacs.el" "$DOTFILES/dot-home/.emacs.el"
handle_file "$HOME/.emacs" "$DOTFILES/dot-home/.emacs"
handle_file "$HOME/.emacs.d" "$DOTFILES/dot-home/.emacs.d"
handle_file "$HOME/.doom.d" "$DOTFILES/dot-home/.doom.d"
handle_file "$HOME/.nanorc" "$DOTFILES/dot-home/.nanorc"


### rename ~/.config/git/config to ~/.gitconfig
# I use ~/.gitconfig as machine-specific configuration, since it has higher
# precedence than ~/.config/git/config, and since I don't track ~/.gitconfig in my
# dotfiles repo.
if [ -f "$HOME/.config/git/config" ] && [ ! -L "$HOME/.config/git" ] && [ ! -L "$HOME/.config/git/config" ]; then
	my_echo "Renaming ~/.config/git/config to ~/.gitconfig"
	if [ -e "$HOME/.gitconfig" ]; then
		my_echo "ERROR: $HOME/.gitconfig already exists"
		exit 1
	fi
	mv -i "$HOME/.config/git/config" "$HOME/.gitconfig"
fi

### rename ~/.ssh/config to ~/.ssh/config.local
# I use ~/.ssh/config.local as machine-specific configuration. I import it from my ~/.ssh/config.
if [ -f "$HOME/.ssh/config" ] && [ ! -L "$HOME/.ssh" ] && [ ! -L "$HOME/.ssh/config" ]; then
	my_echo "Renaming ~/.ssh/config to ~/.ssh/config.local"
	if [ -e "$HOME/.ssh/config.local" ]; then
		my_echo "ERROR: $HOME/.ssh/config.local already exists"
		exit 1
	fi
	mv -i "$HOME/.ssh/config" "$HOME/.ssh/config.local"
fi

## dot-home
handle_file "$HOME" "$DOTFILES/dot-home"
[ ! -d "$DOTFILES/dot-home.private" ] || handle_file "$HOME" "$DOTFILES/dot-home.private"

## config
handle_file "$HOME/.config" "$DOTFILES/config"
[ ! -d "$DOTFILES/config.private" ] || handle_file "$HOME/.config" "$DOTFILES/config.private"

## vscode
if [ "$(uname -s)" = "Darwin" ]; then
	handle_file "$HOME/Library/Application Support/Code/User" "$DOTFILES/vscode"
else
	handle_file "$HOME/.config/Code/User" "$DOTFILES/vscode"
fi

## launchd
if [ "$(uname -s)" = "Darwin" ]; then
	handle_file "$HOME/Library/LaunchAgents" "$DOTFILES/launchd"
	[ ! -d "$DOTFILES/launchd.private" ] || handle_file "$HOME/Library/LaunchAgents" "$DOTFILES/launchd.private"
fi
