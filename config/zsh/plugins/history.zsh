### History
# https://news.ycombinator.com/item?id=33186412
export HISTSIZE=5000000000
export SAVEHIST=1000000000
export HISTFILE="$HOME/.zsh_my_history"

if (( $+commands[awk] )); then
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
