(( ${+commands[mise]} )) && () {
  local command=${commands[mise]}

  eval "$($command activate zsh)"
  # source <($command activate zsh)
  source <($command hook-env -s zsh)

  # generating completions
  local compfile=$1/functions/_mise
  if [[ ! -e $compfile || $compfile -ot $command ]]; then
    $command complete --shell zsh >| $compfile
    # print -u2 -PR "* Detected a new version 'mise'. Regenerated completions."
  fi
} ${0:h}
