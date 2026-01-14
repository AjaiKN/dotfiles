# shellcheck shell=sh

# This file is loaded by ~/.profile, ~/.bash_profile, and ~/.zprofile.

## umask
# # shellcheck disable=SC3028
# if [ "${EUID:-$(id -u)}" -eq 0 ]; then # https://stackoverflow.com/a/52586842
# 	# We may want files created by the superuser to be default-readable by other users
# 	# NOTE: To get sudo to be more permissive than the current umask, you need to add this to /etc/sudoers:
# 	#   Defaults umask_override
# 	#   Defaults umask=0022
# 	umask 022
# else
# 	# Very conservative umask for regular users
# 	umask go-rwx
# fi

# Don't allow obviously insecure umask
umask go-w

## Nix prologue
OLD_PATH_SHELL_PROFILE=$PATH

## env vars
# shellcheck source=./envvars.sh
. "$HOME/.config/shell/envvars.sh"

## shell_env
# shellcheck source=./env.sh
. "$HOME/.config/shell/env.sh"

## Stuff

# from FreeBSD's .profile: Let sh(1) know it's at home, despite /home being a symlink.
# shellcheck disable=2164
if [ "$PWD" != "$HOME" ] && [ "$PWD" -ef "$HOME" ] ; then cd ; fi

## Nix epilogue
if [ -n "$IN_NIX_SHELL" ]; then
	# If we're in a Nix shell, make sure the stuff Nix added to the PATH
	# is ahead of all my stuff.
	PATH=$OLD_PATH_SHELL_PROFILE:$PATH
fi

# shellcheck disable=SC1090
# shellcheck disable=SC1091
if [ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
	. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
elif [ -e "$HOME/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh" ]; then
	. "$HOME/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh"
elif [ -e "/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh" ]; then
	. "/etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh"
fi
