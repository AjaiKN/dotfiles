# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
# https://github.com/jdx/mise?tab=readme-ov-file#ide-integration
# https://stackoverflow.com/questions/73416959/why-vs-code-on-macos-suggests-add-path-to-zprofile-instead-of-zshrc
# https://code.visualstudio.com/docs/setup/mac#_alternative-manual-instructions

if [[ -o interactive ]]; then
	# so we can wait until after the instant prompt loads (in zshrc) before running secure_path
	skip_secure_path=1 source "$HOME/.shell_profile.sh" || return 1
else
	source "$HOME/.shell_profile.sh" || return 1
fi

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path
