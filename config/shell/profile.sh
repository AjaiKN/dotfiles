# shellcheck shell=sh

# This file is loaded by ~/.profile, ~/.bash_profile, and ~/.zprofile.

## umask
umask go-rwx

## Nix prologue
OLD_PATH_SHELL_PROFILE=$PATH

## env vars
. "$HOME/.config/shell/envvars.sh"

## shell_env
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
