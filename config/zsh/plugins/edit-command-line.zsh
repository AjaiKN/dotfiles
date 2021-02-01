### Emacs edit-command-line (control-o)
# https://magnus.therning.org/2023-09-30-using-emacs-as-$editor.html
function my-edit-command-line() {
	zle edit-command-line
	zle redisplay
	zle vi-end-of-line
	if type _vi-mode-set-cursor-shape-for-keymap >/dev/null 2>&1; then
		_vi-mode-set-cursor-shape-for-keymap
	fi
}
zle -N my-edit-command-line

bindkey -M vicmd '^O' my-edit-command-line
bindkey -M viins '^O' my-edit-command-line
bindkey '^O' my-edit-command-line
