function func_rename {
	functions -c $1 $2
	unfunction $1
}

zmodload -m -F zsh/files b:zf_\*

# TODO: read about these functions
autoload -Uz promptinit \
	zsh-mime-setup zsh-mime-handler pick-web-browser \
	zcalc \
	zargs zed zmv \
	colors tetriscurses nslookup regexp-replace

func_rename zed zedit # so zsh's zed doesn't shadow the https://zed.dev/ editor
alias fned='zedit -f'
alias zedit_fn='zedit -f'
alias histed='zedit -h'
alias zedit_hist='zedit -h'

# The zmv function changes its behavior depending on its name
functions -c zmv zcp
functions -c zmv zln

alias zmv="nocorrect noglob zmv -vM -o'-i'"
alias zcp="nocorrect noglob zcp -vM -o'-i'"
alias zln="nocorrect noglob zln -vM"
alias zmov='zmv -w'
alias zcop='zcp -w'
alias zlin='zln -w'
alias zmove='zmv -W'
alias zcopy='zcp -W'
alias zlink='zln -W'
# All these are equivalent:
#   zmove *.oldext *.newext
#   zmov *.oldext '$1'.newext
#   zmv (*).oldext '$1'.newext

(( $+commands[tetris] )) || alias tetris=tetriscurses
