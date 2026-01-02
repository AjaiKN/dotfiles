# shellcheck shell=sh

akn_path_high_priority="\
$HOME/.local/bin:\
$HOME/bin:\
$DOTFILES/private/bin:
$DOTFILES/bin:\
$HOME/.local/share/mise/shims:\
$HOME/.config/emacs/bin:\
/run/wrappers/bin:\
$HOME/.nix-profile/bin:\
/nix/profile/bin:\
$HOME/.local/state/nix/profile/bin:\
/etc/profiles/per-user/$USER/bin:\
/nix/var/nix/profiles/default/bin:\
/run/current-system/sw/bin:\
$HOME/.cargo/bin:\
$HOME/.poetry/bin:\
/Applications/Coq-Platform~8.16~2022.09.app/Contents/Resources/bin"

# then brew

akn_path_mid_priority="\
/opt/homebrew/bin:\
/snap/bin:\
/usr/local/bin:\
/usr/local/sbin:\
/opt/local/bin:\
/opt/local/sbin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin"

# then original $PATH

akn_path_lowest_priority="\
$HOME/.antigravity/antigravity/bin:\
/opt/R/arm64/gfortran/bin:\
/Applications/Firefox.app/Contents/MacOS:\
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support:\
$HOME/prog/plan9port/bin\
"

## putting it all together

export PATH="$akn_path_mid_priority:$PATH:$akn_path_lowest_priority"
# shellcheck source=./brew.sh
. "$HOME/.config/shell/brew.sh"
export PATH="$akn_path_high_priority:$PATH"

unset akn_path_high_priority akn_path_mid_priority akn_path_lowest_priority


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
