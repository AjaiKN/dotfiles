### Mise

(( ${+commands[mise]} )) || return

# mise_oldpath="$PATH"

unset __MISE_ORIG_PATH # Leaving this set has resulted in some confusing behavior sometimes

eval "$(command mise activate zsh)"
# source <($command activate zsh)
source <(command mise hook-env -s zsh)

# PATH="$mise_oldpath"
# unset mise_oldpath
