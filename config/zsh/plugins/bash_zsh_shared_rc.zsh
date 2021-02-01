safe_source "$HOME"/.bash_zsh_shared_rc.sh

# put it at the end of fpath, not the beginning
fpath[$fpath[(i)$HOMEBREW_PREFIX/share/zsh/site-functions]]=()
fpath+=( $HOMEBREW_PREFIX/share/zsh/site-functions )
