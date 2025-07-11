#License for this file:
# MIT License
#
# Copyright (c) 2009-2022 Robby Russell and contributors (https://github.com/ohmyzsh/ohmyzsh/contributors)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

### Aliases
# alias bzr='nocorrect bzr'

function reload {
	# from oh-my-zsh
	# Delete current completion cache
	# command rm -f $_comp_dumpfile $ZSH_COMPDUMP
	# Old zsh versions don't have ZSH_ARGZERO
	local zsh="${ZSH_ARGZERO:-${functrace[-1]%:*}}"
	# Check whether to run a login shell
	[[ "$zsh" = -* || -o login ]] && exec -l "${zsh#-}" || exec "$zsh"
}
alias reload_zshrc='source ~/.zshrc'
alias edit_zshrc='vim ~/.zshrc; reload_zshrc'

alias time_since_last_prompt='echo $P9K_COMMAND_DURATION_SECONDS'

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

# from oh-my-zsh
function d () {
	if [[ -n $1 ]]; then
		dirs "$@"
	else
		dirs -v | head -n 10
	fi
}
