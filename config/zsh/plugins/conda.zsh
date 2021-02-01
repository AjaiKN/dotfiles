### Conda
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/ajainelson/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/ajainelson/mambaforge/etc/profile.d/conda.sh" ]; then
        safe_source "/Users/ajainelson/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/Users/ajainelson/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
