### setopt
# options are case-insensitive; underscores are ignored

#### Changing Directories
setopt auto_cd
setopt auto_param_slash
setopt auto_pushd
setopt cdable_vars
# setopt chase_dots
# setopt chase_links
setopt pushd_ignore_dups # Don’t push multiple copies of the same directory onto the directory stack.
setopt pushd_minus # Exchanges the meanings of ‘+’ and ‘-’ when used with a number to specify a directory in the stack.
#### Completion
setopt complete_in_word     # Complete from both ends of a word.
setopt always_to_end        # Move cursor to the end of a completed word.
setopt path_dirs            # Perform path search even on command names with slashes.
setopt auto_menu            # Show completion menu on a successive tab press.
setopt auto_list            # Automatically list choices on ambiguous completion.
setopt auto_param_slash     # If completed parameter is a directory, add a trailing slash.
unsetopt menu_complete      # Do not autoselect the first completion entry.
unsetopt flow_control       # Disable start/stop characters in shell editor.
# setopt auto_name_dirs
setopt hash_list_all # Whenever a command completion or spelling correction is attempted, make sure the entire command path is hashed first. This makes the first completion slower but avoids false reports of spelling errors.
#### Expansion and Globbing
setopt extended_glob # Treat the ‘#’, ‘~’ and ‘^’ characters as part of patterns for filename generation, etc. (An initial unquoted ‘~’ always produces named directory expansion.)
#### History
setopt extended_history       # record timestamp of command in HISTFILE (format: ': <beginning time>:<elapsed seconds>;<command>')
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_find_no_dups      # Don't display duplicates when searching the history.
setopt hist_ignore_dups       # Don't enter consecutive duplicates into the history.
setopt hist_ignore_space      # remove commands that start with a space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # Cause all terminals to share the same history 'session'.
#### Initialisation
#### Input/Output
setopt correct # Try to correct the spelling of commands.
# setopt correctall # Also try to correct the spelling of all arguments in a line.
setopt interactive_comments # Allow comments starting with `#` in the interactive shell (normally only allowed in scripts).
setopt no_flow_control # disable FLOW_CONTROL (on by default): If this option is unset, output flow control via start/stop characters (usually assigned to ^S/^Q) is disabled in the shell’s editor.
# setopt path_dirs # Perform a path search even on command names with slashes in them. If ‘/usr/local/bin’ is in the user’s path, and he or she types ‘X11/xinit’, the command ‘/usr/local/bin/X11/xinit’ will be executed.
setopt no_clobber # Disallow `>` to overwrite existing files. Use `>|` or `>!` instead.
# setopt no_hup # Prevent SIGHUP to jobs on shell exit.
#### Job Control
setopt long_list_jobs # Print job notifications in the long format by default.
setopt no_bg_nice # disable BG_NICE (set by default): Run all background jobs at a lower priority
#### Prompting
setopt transient_rprompt # Remove the right prompt after each command. Otherwise, when I resize the window, the previous right prompts get misaligned.
#### Scripts and Functions
setopt c_bases # Output hexadecimal numbers in the standard C format, for example ‘0xFF’ instead of the usual ‘16#FF’.
setopt multios # Perform implicit tees or cats when multiple redirections are attempted (see Redirection: https://zsh.sourceforge.io/Doc/Release/Redirection.html#Redirection).
#### Shell Emulation
#### Shell State
#### Zle


### Parameters

# export MANPATH="/usr/local/man:$MANPATH"

export LANG=en_US.UTF-8
