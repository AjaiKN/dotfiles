# see EXPANSION OF PROMPT SEQUENCES in `man zshall``
# see https://github.com/romkatv/zsh4humans/blob/fd5959b3ce6b2b59a377c5d94670f16937495ebb/z4h.zsh#L10-L11

# function my_dir {
#   if [[ "$(dirname "$PWD")" = "$HOME/prog" ]]; then
#     echo '%1~'
#   else
#     echo '%2~'
#   fi
# }

# https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Zsh
# see `vcs_info` in `man zshall`
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats       '[%F{blue}%f%b%F{green}%c%f]%F{red}%u%f '
zstyle ':vcs_info:git:*' actionformats '[%F{blue}%f%b%F{green}%c%f|%a]%F{red}%u%f '
zstyle ':vcs_info:git:*' unstagedstr '*'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:git:*' check-for-changes 1
zstyle ':vcs_info:git:*' use-simple 1 # faster

PS1='%(?.. %F{red}↳ %?%f
)%F{green}%~%f ${vcs_info_msg_0_}%# '
