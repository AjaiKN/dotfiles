### Mise
mise_oldpath="$PATH"

# https://github.com/jdx/mise
hash mise 2>/dev/null && eval "$(mise activate zsh)"

# PATH="$mise_oldpath"
unset mise_oldpath
