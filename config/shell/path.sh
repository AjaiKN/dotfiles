# shellcheck shell=sh

export PATH="\
$HOME/.local/bin:\
$HOME/bin:\
$DOTFILES/private/bin:\
$DOTFILES/bin:\
$HOME/.local/share/mise/shims:\
$HOME/.config/emacs/bin:\
/run/wrappers/bin:\
${GUIX_PROFILE:-$HOME/.guix-profile}/bin:\
$HOME/.guix-profile/bin:\
${XDG_CONFIG_HOME:-$HOME/.config}/guix/current/bin:\
/var/guix/profiles/system/bin:\
$HOME/.nix-profile/bin:\
/nix/profile/bin:\
$HOME/.local/state/nix/profile/bin:\
/etc/profiles/per-user/$USER/bin:\
/nix/var/nix/profiles/default/bin:\
/run/current-system/sw/bin:\
$HOME/.cargo/bin:\
$HOME/.poetry/bin:\
/Applications/Emacs.app/Contents/MacOS/bin:\
/Applications/Coq-Platform~8.16~2022.09.app/Contents/Resources/bin:\
/opt/local/bin:/opt/local/sbin:\
${HOMEBREW_PREFIX:=/opt/homebrew}/bin:\
${HOMEBREW_PREFIX:-/opt/homebrew}/sbin:\
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
$HOMEBREW_PREFIX/opt/trash-cli/bin:\
/opt/R/arm64/gfortran/bin:\
/Applications/Firefox.app/Contents/MacOS:\
/Applications/Gnucash.app/Contents/Resources/bin:\
/Applications/Gnucash.app/Contents/MacOS:\
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:\
$HOME/prog/plan9port/bin"

## Secure PATH
if [ -n "${AKN_BUNDLED:-}" ]; then
	# embedded in the bundle by scripts/bundle
	akn_secure_path=__akn_bundled_secure_path
elif [ -x "$DOTFILES/scripts/secure_path" ]; then
	akn_secure_path="$DOTFILES/scripts/secure_path"
else
	akn_secure_path=
fi
if [ -n "$akn_secure_path" ]; then
	if [ -t 0 ]; then
		PATH="$("$akn_secure_path" || printf '%s' "$PATH")"
	else
		# if not interactive, don't let it print anything
		PATH="$("$akn_secure_path" 2>/dev/null || printf '%s' "$PATH")"
	fi
	export PATH
fi
unset akn_secure_path
