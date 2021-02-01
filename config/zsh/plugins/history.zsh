### History
# https://news.ycombinator.com/item?id=33186412
export HISTSIZE=5000000000
export SAVEHIST=1000000000
export HISTFILE="$HOME/.zsh_my_history"

if [ ! -f ~/.zsh_my_history_backup ]; then
	# echo_if_interactive "Creating ~/.zsh_my_history_backup"
	touch ~/.zsh_my_history_backup
fi
# if ruby is installed
if command -v ruby >/dev/null 2>&1; then
	# echo_if_interactive 'Backing up ~/.zsh_my_history to ~/.zsh_my_history_backup'
	# append all lines in ~/.zsh_my_history that are not in ~/.zsh_my_history_backup to ~/.zsh_my_history_backup
	# Alternatively, this might work (from ChatGPT): awk 'NR==FNR {seen[$0]; next} !($0 in seen)' "$HOME/.zsh_my_history_backup" "$HOME/.zsh_my_history"
	(MISE_NOT_FOUND_AUTO_INSTALL=false ruby -e "puts File.readlines('$HOME/.zsh_my_history') - File.readlines('$HOME/.zsh_my_history_backup')" >> ~/.zsh_my_history_backup &)
	# echo_if_interactive 'Done backing up ~/.zsh_my_history'
else
	echo_if_interactive 'ruby not installed, not backing up ~/.zsh_my_history to ~/.zsh_my_history_backup'
fi
