### History
# https://news.ycombinator.com/item?id=33186412
export HISTSIZE=5000000000
export SAVEHIST=1000000000
if [[ -n $AKN_DOTFILES_UNINSTALLED ]]; then
	export HISTFILE="$DOTFILES/.zsh_my_history"
	# If sourced into an already-running shell, its history came from the old
	# HISTFILE; switch to a fresh history list backed by ours
	[[ -n "$(fc -ln -1 2>/dev/null)" ]] && fc -p "$HISTFILE"
else
	export HISTFILE="$HOME/.zsh_my_history"
fi

if [[ -n $AKN_DOTFILES_UNINSTALLED ]]; then
	: # don't write the backup into $HOME
elif (( $+commands[awk] )); then
	# echo_if_interactive 'Backing up ~/.zsh_my_history to ~/.zsh_my_history_backup'
	(
		umask go-rwx
		# append all lines in ~/.zsh_my_history that are not in ~/.zsh_my_history_backup to ~/.zsh_my_history_backup
		awk 'FILENAME==ARGV[1]{seen[$0]=1; next} !($0 in seen)' "$HOME/.zsh_my_history_backup" "$HOME/.zsh_my_history" >>| "$HOME/.zsh_my_history_backup" &|
	)
	# echo_if_interactive 'Done backing up ~/.zsh_my_history'
else
	echo_if_interactive 'awk not available, not backing up ~/.zsh_my_history to ~/.zsh_my_history_backup'
fi
