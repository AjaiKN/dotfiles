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

## System clipboard integration - COPIED FROM OH-MY-ZSH
#
# This file has support for doing system clipboard copy and paste operations
# from the command line in a generic cross-platform fashion.
#
# This is uses essentially the same heuristic as neovim, with the additional
# special support for Cygwin.
# See: https://github.com/neovim/neovim/blob/e682d799fa3cf2e80a02d00c6ea874599d58f0e7/runtime/autoload/provider/clipboard.vim#L55-L121
#
# - pbcopy, pbpaste (macOS)
# - cygwin (Windows running Cygwin)
# - wl-copy, wl-paste (if $WAYLAND_DISPLAY is set)
# - xsel (if $DISPLAY is set)
# - xclip (if $DISPLAY is set)
# - lemonade (for SSH) https://github.com/pocke/lemonade
# - doitclient (for SSH) http://www.chiark.greenend.org.uk/~sgtatham/doit/
# - win32yank (Windows)
# - tmux (if $TMUX is set)
#
# Defines two functions, clipcopy and clippaste, based on the detected platform.
##
#
# clipcopy - Copy data to clipboard
#
# Usage:
#
#  <command> | clipcopy    - copies stdin to clipboard
#
#  clipcopy <file>         - copies a file's contents to clipboard
#
##
#
# clippaste - "Paste" data from clipboard to stdout
#
# Usage:
#
#   clippaste   - writes clipboard's contents to stdout
#
#   clippaste | <command>    - pastes contents and pipes it to another process
#
#   clippaste > <file>      - paste contents to a file
#
# Examples:
#
#   # Pipe to another process
#   clippaste | grep foo
#
#   # Paste to a file
#   clippaste > file.txt
#
function detect-clipboard() {
	emulate -L zsh

	if [[ "${OSTYPE}" == darwin* ]] && (( ${+commands[pbcopy]} )) && (( ${+commands[pbpaste]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | pbcopy; }
		function clippaste() { pbpaste; }
	elif [[ "${OSTYPE}" == (cygwin|msys)* ]]; then
		function clipcopy() { cat "${1:-/dev/stdin}" > /dev/clipboard; }
		function clippaste() { cat /dev/clipboard; }
	elif (( $+commands[clip.exe] )) && (( $+commands[powershell.exe] )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | clip.exe; }
		function clippaste() { powershell.exe -noprofile -command Get-Clipboard; }
	elif [ -n "${WAYLAND_DISPLAY:-}" ] && (( ${+commands[wl-copy]} )) && (( ${+commands[wl-paste]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | wl-copy --type text/plain &>/dev/null &|; }
		function clippaste() { wl-paste --no-newline | cat; }
	elif [ -n "${DISPLAY:-}" ] && (( ${+commands[xsel]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | xsel --clipboard --input; }
		function clippaste() { xsel --clipboard --output; }
	elif [ -n "${DISPLAY:-}" ] && (( ${+commands[xclip]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | xclip -selection clipboard -in &>/dev/null &|; }
		function clippaste() { xclip -out -selection clipboard; }
	elif (( ${+commands[lemonade]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | lemonade copy; }
		function clippaste() { lemonade paste; }
	elif (( ${+commands[doitclient]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | doitclient wclip; }
		function clippaste() { doitclient wclip -r; }
	elif (( ${+commands[win32yank]} )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | win32yank -i; }
		function clippaste() { win32yank -o; }
	elif [[ $OSTYPE == linux-android* ]] && (( $+commands[termux-clipboard-set] )); then
		function clipcopy() { cat "${1:-/dev/stdin}" | termux-clipboard-set; }
		function clippaste() { termux-clipboard-get; }
	elif [ -n "${TMUX:-}" ] && (( ${+commands[tmux]} )); then
		function clipcopy() { tmux load-buffer -w "${1:--}"; }
		function clippaste() { tmux save-buffer -; }
	else
		function _retry_clipboard_detection_or_fail() {
			local clipcmd="${1}"; shift
			if detect-clipboard; then
				"${clipcmd}" "$@"
			else
				print "${clipcmd}: Platform $OSTYPE not supported or xclip/xsel not installed" >&2
				return 1
			fi
		}
		function clipcopy() { _retry_clipboard_detection_or_fail clipcopy "$@"; }
		function clippaste() { _retry_clipboard_detection_or_fail clippaste "$@"; }
		return 1
	fi
}

function clipcopy clippaste {
	unfunction clipcopy clippaste
	detect-clipboard || true # let one retry
	"$0" "$@"
}

if [[ "${OSTYPE}" != darwin* ]]; then
	alias pbcopy=clipcopy
	alias pbpaste=clippaste
fi

### ohmyzsh copypath plugin

# Copies the path of given directory or file to the system or X Windows clipboard.
# Copy current directory if no parameter.
function copypath {
	# If no argument passed, use current directory
	local file="${1:-.}"

	# If argument is not an absolute path, prepend $PWD
	[[ $file = /* ]] || file="$PWD/$file"

	# Copy the absolute path without resolving symlinks
	# If clipcopy fails, exit the function with an error
	print -n "${file:a}" | clipcopy || return 1

	echo ${(%):-"%B${file:a}%b copied to clipboard."}
}

## me: make the rest of the copying commands use the system clipboard
# This is largely copied from the ohmyzsh-vi-mode plugin, which only does this for the vi commands:
#   https://github.com/ohmyzsh/ohmyzsh/blob/f17aa2ffa8c12b71518f1b0233edca3a0dd7cade/plugins/vi-mode/vi-mode.plugin.zsh#L121-L162
# TODO: integrate yank-pop with emacs kill ring if vterm.
# TODO: maybe ONLY do the non-vi ones if we're inside vterm?
# TODO: set up vterm to use the zsh keyboard (if we're at the prompt? is that possible to detect?)
# TODO: remove duplicates from kill ring
function wrap_clipboard_widgets() {
	# NB: Assume we are the first wrapper and that we only wrap native widgets
	# See zsh-autosuggestions.zsh for a more generic and more robust wrapper
	local verb="$1"
	shift

	local widget
	local wrapped_name
	for widget in "$@"; do
		wrapped_name="_zsh-vi-${verb}-${widget}"
		if [ "${verb}" = copy ]; then
			eval "
				function ${wrapped_name}() {
					# If the system clipboard isn't empty and it's different from the current CUTBUFFER, then
					# copy the thing on the system clipboard to the killring so we don't completely overwrite it.
					SYSCLIP=\"\$(clippaste 2>/dev/null || echo \$CUTBUFFER)\"
					if [ -n \"\$SYSCLIP\" ]; then
						if [ \"\$SYSCLIP\" != \"\$CUTBUFFER\" ]; then
							zle .copy-region-as-kill \"\$SYSCLIP\" 2>/dev/null || true
						fi
					fi
					# call the original widget
					zle .${widget}
					# copy the thing on the system clipboard
					if [ -n \"\$CUTBUFFER\" ]; then
						 printf %s \"\${CUTBUFFER}\" | clipcopy 2>/dev/null || true
					fi

					# see below for explanation
					if [[ ! "${ZSH_VERSION}" < 5.2 ]]; then
						zle -f kill
					fi
				}
			"
		else
			# paste or paste_before
			eval "
				function ${wrapped_name}() {
					# If the system clipboard isn't empty and it's different from the current CUTBUFFER, then
					# copy the thing on the system clipboard to the CUTBUFFER so it'll get pasted.
					SYSCLIP=\"\$(clippaste 2>/dev/null || echo \$CUTBUFFER)\"
					if [ -n \"\$SYSCLIP\" ]; then
						if [ \"\$SYSCLIP\" != \"\$CUTBUFFER\" ]; then
							zle .copy-region-as-kill \"\$SYSCLIP\" 2>/dev/null || true
						fi
					fi
					zle .${widget}

					# either zle -f yank or zle -f yankbefore (depending on if it's paste or paste_before).
					# I'm not sure how important that difference actually is, though.
					if [[ ! "${ZSH_VERSION}" < 5.2 ]]; then
						if [ \"$widget\" = \"vi-put-before\" ]; then
							zle -f yankbefore
						else
							zle -f yank
						fi
					fi
				}
			"
			#     ^^^^^^^^^^^
			# NOTE: overriding without doing "zle -f yank" causes problems with yank-pop:
			#   SOLUTION: https://github.com/zsh-users/zsh-autosuggestions/pull/551;
			#   https://github.com/zsh-users/zsh-syntax-highlighting/issues/99,
			#   https://github.com/zsh-users/zsh-syntax-highlighting/commit/766b350a7d346374e8945a0644825953fc202181,
			#   https://github.com/zsh-users/zsh-autosuggestions/issues/363#issuecomment-449649645,
			#   https://github.com/danielshahaf/zsh-syntax-highlighting/commit/bfa71c983fa6c3b43cc657276223410123d5c145
		fi
		zle -N "${widget}" "${wrapped_name}"
	done
}

if [[ -z "${VI_MODE_DISABLE_CLIPBOARD:-}" ]]; then
	wrap_clipboard_widgets copy \
		vi-yank vi-yank-eol vi-yank-whole-line \
		vi-change vi-change-eol vi-change-whole-line \
		vi-kill-line vi-kill-eol vi-backward-kill-word \
		vi-delete vi-delete-char vi-backward-delete-char \
		backward-kill-line backward-kill-word copy-region-as-kill kill-buffer kill-line kill-region kill-whole-line kill-word \
		backward-delete-word delete-word
		# backward-delete-char delete-char delete-char-or-list
	wrap_clipboard_widgets paste \
		vi-put-after vi-put-before \
		put-replace-selection \
		yank

	# NOTE: it's fine to override yank-pop too.
	# I might want to do that to integrate it with the emacs kill ring.

	unfunction wrap_clipboard_widgets
fi

VI_MODE_DISABLE_CLIPBOARD=1
