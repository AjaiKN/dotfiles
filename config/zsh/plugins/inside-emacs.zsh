### Emacs vterm integration
if [[ "$INSIDE_EMACS" = "vterm"* ]] && [[ -n ${EMACS_VTERM_PATH} ]] && { ! [[ "$TERM" = "dumb" ]] } && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
	# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration-files
	# https://github.com/akermu/emacs-libvterm/issues/677
	# https://github.com/akermu/emacs-libvterm/blob/master/etc/emacs-vterm-zsh.sh
	# safe_source ~/.my-emacs-vterm-zsh.sh
	safe_source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

	# https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#how-can-i-get-the-directory-tracking-in-a-more-understandable-way
	# The implementation in emacs-vterm-zsh.sh puts it into the prompt, which I think doesn't work because of p10k.
	vterm_set_directory() {
		vterm_cmd update-pwd "$PWD/"
		# On a remote one, use instead:
		# vterm_cmd update-pwd "/-:""$USER""@""$HOSTNAME"":""$PWD/"
	}
	autoload -U add-zsh-hook
	add-zsh-hook -Uz chpwd (){ vterm_set_directory }

	# Use vterm_cmd to send commands to the vterm buffer:
	#   vterm_cmd find-file $(realpath my_file.txt)
	#   vterm_cmd message "Hello from vterm"
	# To enable new commands, you have to customize Emacs's variable vterm-eval-cmds.
	unalias em
	function em () {
		vterm_cmd find-file-other-window "$(realpath "${@:-.}")"
	}
fi

### Emacs eat integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && safe_source "$EAT_SHELL_INTEGRATION_DIR/zsh" && __eat_enable_integration && TERM=xterm-256color

### Emacs mistty
if [[ "$TERM" = "eterm-color" ]]; then
	ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#666666"

	zmodload zsh/complist
	# https://unix.stackexchange.com/questions/84867/zsh-completion-enabling-shift-tab
	bindkey -M menuselect '^[[Z' reverse-menu-complete

	# Disable selection from the autocomplete menu.
	# setopt noautomenu
fi

