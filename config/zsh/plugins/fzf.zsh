### fzf
if command -v fzf >/dev/null 2>&1; then
	# Set up fzf key bindings and fuzzy completion
	source <(fzf --zsh)
fi
