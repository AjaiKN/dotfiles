### don't autocorrect to things starting with underscore
# https://stackoverflow.com/a/61845653
# Zsh variable to determine what to ignore,
# in this case everything starting with _ or .
CORRECT_IGNORE="[_|.]*"
# setopt HASH_LIST_ALL
# setopt hashlistall
# zstyle ":completion:*:commands" rehash 1
# setopt CORRECT
