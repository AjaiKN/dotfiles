### Key bindings
# alt-left and alt-right
if [ "$TERM" != "eterm-color" ]; then
	# run `bindkey -LM emacs` to see emacs bindings
  # run `bindkey -LM viins` to see vi insert bindings
	bindkey "^@" set-mark-command
	bindkey "^A" beginning-of-line
	bindkey "^B" backward-char
	# bindkey "^D" delete-char-or-list
	bindkey "^E" end-of-line
	bindkey "^F" forward-char
	bindkey "^G" send-break
	# bindkey "^H" backward-delete-char
	# bindkey "^I" expand-or-complete # fzf-completion
	bindkey "^J" accept-line
	bindkey "^K" kill-line
	bindkey "^L" clear-screen
	bindkey "^M" accept-line
	bindkey "^N" down-line-or-history # or down-history?
	# bindkey "^O" accept-line-and-down-history
	bindkey "^P" up-line-or-history # or up-history?
	bindkey "^Q" push-line
	bindkey "^R" fzf-history-widget
	bindkey "^S" history-incremental-search-forward
	bindkey "^T" fzf-file-widget
  #Conflict: In vim, this deletes until the beginning of the line, not the whole line
	# bindkey "^U" kill-whole-line
	bindkey "^V" quoted-insert
	bindkey "^W" backward-kill-word
	bindkey "^X^B" vi-match-bracket
	bindkey "^X^E" edit-command-line
	bindkey "^X^F" vi-find-next-char
	bindkey "^X^J" vi-join
	bindkey "^X^K" kill-buffer
	bindkey "^X^N" infer-next-history
	bindkey "^X^O" overwrite-mode
	bindkey "^X^R" _read_comp
	bindkey "^X^U" undo
	bindkey "^X^V" vi-cmd-mode
	bindkey "^X^X" exchange-point-and-mark
	bindkey "^X*" expand-word
	bindkey "^X=" what-cursor-position
	bindkey "^X?" _complete_debug
	bindkey "^XC" _correct_filename
	bindkey "^XG" list-expand
	bindkey "^Xa" _expand_alias
	bindkey "^Xc" _correct_word
	bindkey "^Xd" _list_expansions
	bindkey "^Xe" _expand_word
	bindkey "^Xg" list-expand
	bindkey "^Xh" _complete_help
	bindkey "^Xm" _most_recent_file
	bindkey "^Xn" _next_tags
	bindkey "^Xr" history-incremental-search-backward
	bindkey "^Xs" history-incremental-search-forward
	bindkey "^Xt" _complete_tag
	bindkey "^Xu" undo
	bindkey "^X~" _bash_list-choices
	bindkey "^Y" yank
	bindkey "^[^D" list-choices
	bindkey "^[^G" send-break
	bindkey "^[^H" backward-kill-word
	bindkey "^[^I" self-insert-unmeta
	bindkey "^[^J" self-insert-unmeta
	bindkey "^[^L" clear-screen
	bindkey "^[^M" self-insert-unmeta
	bindkey "^[^_" copy-prev-word
	bindkey "^[ " expand-history
	bindkey "^[!" expand-history
	bindkey "^[\"" quote-region
	bindkey "^[\$" spell-word
	bindkey "^['" quote-line
	bindkey "^[," _history-complete-newer
	bindkey "^[-" neg-argument
	bindkey "^[." insert-last-word
	bindkey "^[/" _history-complete-older
	bindkey "^[0" digit-argument
	bindkey "^[1" digit-argument
	bindkey "^[2" digit-argument
	bindkey "^[3" digit-argument
	bindkey "^[4" digit-argument
	bindkey "^[5" digit-argument
	bindkey "^[6" digit-argument
	bindkey "^[7" digit-argument
	bindkey "^[8" digit-argument
	bindkey "^[9" digit-argument
	bindkey "^[<" beginning-of-buffer-or-history
	bindkey "^[>" end-of-buffer-or-history
	bindkey "^[?" which-command
	bindkey "^[A" accept-and-hold
	bindkey "^[B" backward-word
	bindkey "^[C" capitalize-word
	bindkey "^[D" kill-word
	bindkey "^[F" forward-word
	bindkey "^[G" get-line
	bindkey "^[H" run-help
	bindkey "^[L" down-case-word
	bindkey "^[N" history-search-forward
	# bindkey "^[OA" up-line-or-search # history-substring-search-up
	# bindkey "^[OB" down-line-or-search # history-substring-search-down
	bindkey "^[OC" forward-char
	bindkey "^[OD" backward-char
	bindkey "^[OF" end-of-line
	bindkey "^[OH" beginning-of-line
	bindkey "^[P" history-search-backward
	bindkey "^[Q" push-line
	bindkey "^[S" spell-word
	bindkey "^[T" transpose-words
	bindkey "^[U" up-case-word
	bindkey "^[W" copy-region-as-kill
	bindkey "^[[1;5C" forward-word
	bindkey "^[[1;5D" backward-word
	bindkey "^[[200~" bracketed-paste
	bindkey "^[[3;5~" kill-word
	bindkey "^[[3~" delete-char
	bindkey "^[[5~" up-line-or-history
	bindkey "^[[6~" down-line-or-history
	# bindkey "^[[A" up-line-or-history # history-substring-search-up
	# bindkey "^[[B" down-line-or-history # history-substring-search-down
	# bindkey "^[[C" forward-char # vi-forward-char
	# bindkey "^[[D" backward-char # vi-backward-char
	bindkey "^[[Z" reverse-menu-complete
	bindkey "^[_" insert-last-word
	bindkey "^[a" accept-and-hold
	# bindkey "^[b" backward-word # emacs-backward-word
	bindkey "^[c" fzf-cd-widget
	bindkey "^[d" kill-word # backward-kill-word
	# bindkey "^[f" forward-word # emacs-forward-word
	bindkey "^[g" get-line
	bindkey "^[h" run-help
	bindkey "^[l" down-case-word
	bindkey "^[m" copy-prev-shell-word
	bindkey "^[n" history-search-forward
	bindkey "^[p" history-search-backward
	bindkey "^[q" push-line
	bindkey "^[s" spell-word
	bindkey "^[t" transpose-words
	bindkey "^[u" up-case-word
	bindkey "^[w" kill-region
	bindkey "^[x" execute-named-cmd
	bindkey "^[y" yank-pop
	bindkey "^[z" execute-last-named-cmd
	bindkey "^[|" vi-goto-column
	bindkey "^[~" _bash_complete-word
  # alt-backspace
	bindkey "^[^?" backward-kill-word
	bindkey "^_" undo
	bindkey " " magic-space
	# bindkey -R "!"-"~" self-insert
	bindkey "^?" backward-delete-char
	# bindkey -R "\M-^@"-"\M-^?" self-insert

	bindkey '^[^[[C' emacs-forward-word
	bindkey '^[[1;3C' emacs-forward-word
	bindkey '^[^[[D' emacs-backward-word
	bindkey '^[[1;3D' emacs-backward-word
	bindkey '^[b' emacs-backward-word
	bindkey '^[f' emacs-forward-word

	bindkey '[OA' history-substring-search-up
	bindkey '[OB' history-substring-search-down
	bindkey '^[[A' history-substring-search-up
	bindkey '^[[B' history-substring-search-down
	[[ -n "$terminfo[kcuu1]" ]] && bindkey "$terminfo[kcuu1]" history-substring-search-up
	[[ -n "$terminfo[kcud1]" ]] && bindkey "$terminfo[kcud1]" history-substring-search-down
	# bindkey -M emacs '^P' history-substring-search-up
	# bindkey -M emacs '^N' history-substring-search-down
	# bindkey -M vicmd 'k' history-substring-search-up
	# bindkey -M vicmd 'j' history-substring-search-down
fi

zmodload zsh/complist
# https://old.reddit.com/r/zsh/comments/w8su9x/make_zsh_tab_completion_menu_close_and_cancel/
# bindkey -M menuselect '^[' undo
# https://thevaluable.dev/zsh-completion-guide-examples/#the-module-complist
bindkey -M menuselect '^[' send-break
# bindkey -M menuselect '^xi' vi-insert
bindkey -M menuselect '^G' send-break
