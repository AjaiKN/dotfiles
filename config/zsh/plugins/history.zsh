### History
# https://news.ycombinator.com/item?id=33186412
HISTSIZE=5000000000
SAVEHIST=1000000000
HISTFILE="$HOME/.zsh_my_history"
# Not exported (and un-exported if inherited that way), so that child shells
# like bash don't read and write zsh's history file
typeset -g +x HISTSIZE SAVEHIST HISTFILE

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
