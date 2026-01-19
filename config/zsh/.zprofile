# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
# https://github.com/jdx/mise?tab=readme-ov-file#ide-integration
# https://stackoverflow.com/questions/73416959/why-vs-code-on-macos-suggests-add-path-to-zprofile-instead-of-zshrc
# https://code.visualstudio.com/docs/setup/mac#_alternative-manual-instructions

source "$HOME/.config/shell/profile.sh" || return 1

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

export AKN_INSIDE_ZSH=1
