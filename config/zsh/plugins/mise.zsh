### Mise

(( ${+commands[mise]} )) || return

# mise_oldpath="$PATH"

eval "$(command mise activate zsh)"
# source <($command activate zsh)
source <(command mise hook-env -s zsh)

# PATH="$mise_oldpath"
# unset mise_oldpath
