# shellcheck shell=sh

export PATH="\
$HOME/.local/bin:\
$HOME/bin:\
$DOTFILES/private/bin:\
$DOTFILES/bin:\
$HOME/.local/share/mise/shims:\
$HOME/.config/emacs/bin:\
/run/wrappers/bin:\
${XDG_CONFIG_HOME:-$HOME/.config}/guix/current/bin:\
$HOME/.nix-profile/bin:\
/nix/profile/bin:\
$HOME/.local/state/nix/profile/bin:\
/etc/profiles/per-user/$USER/bin:\
/nix/var/nix/profiles/default/bin:\
/run/current-system/sw/bin:\
$HOME/.cargo/bin:\
$HOME/.poetry/bin:\
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
$HOME/.antigravity/antigravity/bin:\
/opt/R/arm64/gfortran/bin:\
/Applications/Firefox.app/Contents/MacOS:\
/Applications/Gnucash.app/Contents/Resources/bin:\
/Applications/Gnucash.app/Contents/MacOS:\
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:\
$HOME/prog/plan9port/bin"

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
