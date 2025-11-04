#!/bin/sh

set -eu

umask go-rwx

DOTFILES=$(realpath "$(dirname "$0")")
export DOTFILES

cd "$DOTFILES"

dotfiles_relative_to_dir() {
	# NOTE: dir must exist
	[ -d "$1" ] || exit 10
	dir=$(realpath "$1" || exit 21)
	res=""
	real_home=$(realpath "$HOME" || exit 22)
	while true; do
		if [ "$dir" = "/" ]; then
			# printf "%s" "${res}${DOTFILES}"
			# fall back to absolute path
			printf "%s" "${DOTFILES}"
			return
		elif [ "$dir" = "$real_home" ] || [ "$dir" = "$HOME" ]; then
			trimmed=$(echo "$DOTFILES" | sed "s|^$real_home/||" | sed "s|^$HOME/||")
			printf "%s" "${res}${trimmed}"
			return
		else
			res="../$res"
			dir=$(dirname "$dir" || exit 23)
		fi
	done
}

get_planned_link_source() {
	case "$2" in
		"$DOTFILES"/*)
			base=$(dotfiles_relative_to_dir "$(dirname "$1" || exit 27)" || exit 24)
			trimmed=$(echo "$2" | sed "s|^$DOTFILES/||" || exit 26)
			printf "%s/%s" "$base" "$trimmed"
			;;
		/*)
			printf "%s" "$2"
			;;
		*)
			echo "$1 should be an absolute path" >&2
			exit 36
			;;
	esac
}

is_dotfiles_link() {
	[ -L "$1" ] || return 1
	relative=$(dotfiles_relative_to_dir "$(dirname "$1" || exit 28)" || exit 25)
	link=$(readlink "$1" || exit 29)
	case "$link" in
		"$DOTFILES"/*|"$relative"/*|./"$relative"/*) return 0 ;;
		*) return 1 ;;
	esac
}

is_self_link() {
	[ -L "$1" ] || return 1
	link=$(readlink "$1" || exit 30)
	base=$(basename "$1" || exit 31)
	[ "$link" = "$base" ] || [ "$link" = "$1" ]
}

make_backup() {
	if [ -e "$1" ]; then
		make_backup "$1.bak" || exit 32 # If $1.bak already exists, make $1.bak.bak
		my_echo "Renaming $1 to $1.bak"
		mv -i "$1" "$1.bak" || exit 33
	fi
}

is_ignored() {
	base=$(basename "$1" || exit 34)
	case "$base" in
		.DS_Store|.unfold|.#*|*~|'#'*'#') return 0 ;;
		*) return 1 ;;
	esac
}

is_unfold() {
	[ -e "$1/.unfold" ] # && ! [ -L "$1" ]
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
		if is_dotfiles_link "$1" || is_self_link "$1" || [ "$(readlink "$1")" = "$(get_planned_link_source "$1" "$2")" ]; then
			if is_ignored "$1"; then
				my_echo "${red}UNLINK (because ignored): $1 -> $(readlink "$1")${reset}"
				rm "$1"
			elif [ -e "$2" ] && [ "$(readlink "$1")" = "$(get_planned_link_source "$1" "$2")" ] && ! is_unfold "$2"; then
				[ -z "$verbose" ] || my_echo "ALREADY LINKED: $1 -> $(readlink "$1")"
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
		[ -z "$verbose" ] || my_echo "IGNORING: $2"
		return
	fi

	if is_unfold "$2"; then
		mkdir -p "$1"
		my_echo "${blue}UNFOLDING: $1/ -> $(dotfiles_relative_to_dir "$1")/$(echo "$2" | sed "s|^$DOTFILES/||")/${reset}"
		(
			# shellcheck disable=SC2030
			indent="  $indent"

			cd "$2" || exit 50
			for file in * .*; do
				if ! [ "$file" = "." ] && ! [ "$file" = ".." ]; then
					handle_file "$1/$file" "$2/$file"
				fi
			done

			[ -z "$verbose" ] || my_echo "Checking for orphaned symlinks in $1..."
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

## parse arguments

verbose=
while [ $# -gt 0 ]; do
	case "$1" in
		-v|--verbose)
			verbose=1
			shift
			;;
		*)
			echo "ERROR: Unknown option: $1" >&2
			exit 1
			;;
	esac
done

## Uninstalling

uninstall_file() {
	if [ -L "$1" ]; then
		if is_dotfiles_link "$1" || is_self_link "$1" || [ "$(readlink "$1")" = "$(get_planned_link_source "$1" "$2")" ]; then
			my_echo "${red}UNLINK: $1 -> $(readlink "$1")${reset}"
			rm "$1"
		fi
	fi

	if is_ignored "$1"; then
		my_echo "IGNORING: $2"
		return
	fi

	if is_unfold "$2" && [ -d "$1" ]; then
		my_echo "${blue}UNFOLDING: $1/ -> $(dotfiles_relative_to_dir "$1")/$(echo "$2" | sed "s|^$DOTFILES/||")/${reset}"
		(
			# shellcheck disable=SC2031
			indent="  $indent"

			my_echo "Checking for dotfiles symlinks in $1..."
			cd "$1" || exit 51
			for file in * .*; do
				if ! [ "$file" = "." ] && ! [ "$file" = ".." ]; then
					uninstall_file "$1/$file" "$2/$file"
				fi
			done
		)
	fi
}

uninstall() {
	./submodules-update.sh || :
	## dot-home
	uninstall_file "$HOME" "$DOTFILES/dot-home"
	uninstall_file "$HOME" "$DOTFILES/private/dot-home"
	## config
	uninstall_file "$HOME/.config" "$DOTFILES/config"
	uninstall_file "$HOME/.config" "$DOTFILES/private/config"
	## macOS Application Support
	uninstall_file "$HOME/Library/Application Support/Code" "$DOTFILES/config/Code"
	uninstall_file "$HOME/Library/Application Support/edir-flags.conf" "$DOTFILES/config/edir-flags.conf"
	## launchd
	uninstall_file "$HOME/Library/LaunchAgents" "$DOTFILES/launchd"
	uninstall_file "$HOME/Library/LaunchAgents" "$DOTFILES/private/launchd"
	## xbar
	uninstall_file "$HOME/Library/Application Support/xbar/plugins" "$DOTFILES/xbar"
	uninstall_file "$HOME/Library/Application Support/xbar/plugins" "$DOTFILES/private/xbar"

	## rename ~/.ssh/config.local back to ~/.ssh/config
	# I use ~/.ssh/config.local as machine-specific configuration. I import it from my ~/.ssh/config.
	if [ -f "$HOME/.ssh/config.local" ] && [ ! -L "$HOME/.ssh" ] && [ ! -L "$HOME/.ssh/config.local" ] && [ ! -e "$HOME/.ssh/config" ]; then
		my_echo "Renaming ~/.ssh/config.local to ~/.ssh/config"
		mv -i "$HOME/.ssh/config.local" "$HOME/.ssh/config"
	fi
}

if [ -n "${AKN_UNINSTALL:-}" ]; then
	my_echo "Uninstalling symlinks to $DOTFILES..."
	uninstall
	exit
fi

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
[ ! -d "$DOTFILES/private/dot-home" ] || handle_file "$HOME" "$DOTFILES/private/dot-home"

## config
handle_file "$HOME/.config" "$DOTFILES/config"
[ ! -d "$DOTFILES/private/config" ] || handle_file "$HOME/.config" "$DOTFILES/private/config"

## macOS Application Support
if [ "$(uname -s)" = "Darwin" ]; then
	handle_file "$HOME/Library/Application Support/Code" "$DOTFILES/config/Code"
	handle_file "$HOME/Library/Application Support/edir-flags.conf" "$DOTFILES/config/edir-flags.conf"
fi

## launchd
if [ "$(uname -s)" = "Darwin" ]; then
	handle_file "$HOME/Library/LaunchAgents" "$DOTFILES/launchd"
	[ ! -d "$DOTFILES/private/launchd" ] || handle_file "$HOME/Library/LaunchAgents" "$DOTFILES/private/launchd"
fi

## xbar
if [ "$(uname -s)" = "Darwin" ]; then
	[ ! -d "$DOTFILES/xbar" ] || handle_file "$HOME/Library/Application Support/xbar/plugins" "$DOTFILES/xbar"
	[ ! -d "$DOTFILES/private/xbar" ] || handle_file "$HOME/Library/Application Support/xbar/plugins" "$DOTFILES/private/xbar"
fi
