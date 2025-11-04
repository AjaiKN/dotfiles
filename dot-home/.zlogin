# https://unix.stackexchange.com/questions/71253/what-should-shouldnt-go-in-zshenv-zshrc-zlogin-zprofile-zlogout
#
# .zlogin is sourced in login shells.  It should
# contain commands that should be executed only in
# login shells.  It should be used to set the terminal
# type and run a series of external commands (fortune,
# msgs, from, etc).
# (https://github.com/zsh-users/zsh/blob/master/StartupFiles/zlogin)
#
