#!/usr/bin/env bash

set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
set -x

# https://scriptingosx.com/2020/06/about-bash-zsh-sh-and-dash-in-macos-catalina-and-beyond/

#  && [[ $- == *i* ]]
if [ /var/select/sh -ef /bin/bash ] && [ -f /bin/dash ]; then
	set +x
	echo
	echo 'sh is symlinked to bash system-wide.'
	echo "dash is faster than bash and contains fewer non-POSIX features (which makes it easier to catch non-POSIX-compatible scripts)."
	echo 'See https://scriptingosx.com/2020/06/about-bash-zsh-sh-and-dash-in-macos-catalina-and-beyond/'

	# https://stackoverflow.com/a/1885534
	read -p "Symlink the default sh to dash system-wide instead (requires sudo)? (y/n) " -n 1 -r
	echo    # (optional) move to a new line
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		set -x
		sudo ln -sf /bin/dash /var/select/sh
		echo "Done"
	else
		echo "Aborting"
	fi
fi
if [[ $(stat -f '%Lp' /var/select/sh) != 755 ]]; then
	sudo chmod -h 755 /var/select/sh
fi
