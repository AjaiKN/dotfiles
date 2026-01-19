# shellcheck shell=sh

if [ -n "${GUIX_PROFILE:-}" ]; then
	orig_profile="$GUIX_PROFILE"
fi

for the_guix_profile in "/var/guix/profiles/system" "${XDG_CONFIG_HOME:-$HOME/.config}/guix/current" "$HOME/.guix-profile"; do
	if [ -e "$the_guix_profile"/etc/profile ]; then
		GUIX_PROFILE="$the_guix_profile"
		. "$GUIX_PROFILE/etc/profile"
	fi
done
unset the_guix_profile

if [ -n "$orig_profile" ]; then
	GUIX_PROFILE="$orig_profile"
	. "$GUIX_PROFILE/etc/profile"
	unset orig_profile
fi

