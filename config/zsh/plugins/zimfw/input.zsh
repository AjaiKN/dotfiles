#License for this file:
# MIT License
#
# Copyright (c) 2015-2016 Matt Hamilton and contributors
# Copyright (c) 2016-2023 Eric Nielsen, Matt Hamilton and contributors
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

#
# Editor and input char assignment
#

[[ ${TERM} != dumb ]] && () {

  # Use human-friendly identifiers.
  zmodload -F zsh/terminfo +b:echoti +p:terminfo
  typeset -gA key_info
  key_info=(
    Control      '\C-'
    ControlLeft  '\e[1;5D \e[5D \e\e[D \eOd \eOD'
    ControlRight '\e[1;5C \e[5C \e\e[C \eOc \eOC'
    Escape       '\e'
    Meta         '\M-'
    Backspace    '^?'
    Delete       '^[[3~'
    BackTab      "${terminfo[kcbt]}"
    Left         "${terminfo[kcub1]}"
    Down         "${terminfo[kcud1]}"
    Right        "${terminfo[kcuf1]}"
    Up           "${terminfo[kcuu1]}"
    End          "${terminfo[kend]}"
    F1           "${terminfo[kf1]}"
    F2           "${terminfo[kf2]}"
    F3           "${terminfo[kf3]}"
    F4           "${terminfo[kf4]}"
    F5           "${terminfo[kf5]}"
    F6           "${terminfo[kf6]}"
    F7           "${terminfo[kf7]}"
    F8           "${terminfo[kf8]}"
    F9           "${terminfo[kf9]}"
    F10          "${terminfo[kf10]}"
    F11          "${terminfo[kf11]}"
    F12          "${terminfo[kf12]}"
    Home         "${terminfo[khome]}"
    Insert       "${terminfo[kich1]}"
    PageDown     "${terminfo[knp]}"
    PageUp       "${terminfo[kpp]}"
  )

  # Bind the keys

  local key
  for key (${(s: :)key_info[ControlLeft]}) bindkey ${key} backward-word
  for key (${(s: :)key_info[ControlRight]}) bindkey ${key} forward-word

  bindkey ${key_info[Backspace]} backward-delete-char
  bindkey ${key_info[Delete]} delete-char

  if [[ -n ${key_info[Home]} ]] bindkey ${key_info[Home]} beginning-of-line
  if [[ -n ${key_info[End]} ]] bindkey ${key_info[End]} end-of-line

  if [[ -n ${key_info[PageUp]} ]] bindkey ${key_info[PageUp]} up-line-or-history
  if [[ -n ${key_info[PageDown]} ]] bindkey ${key_info[PageDown]} down-line-or-history

  if [[ -n ${key_info[Insert]} ]] bindkey ${key_info[Insert]} overwrite-mode

  if [[ -n ${key_info[Left]} ]] bindkey ${key_info[Left]} backward-char
  if [[ -n ${key_info[Right]} ]] bindkey ${key_info[Right]} forward-char

  # Expandpace.
  bindkey ' ' magic-space

  # Bind insert-last-word even in viins mode
  bindkey "${key_info[Escape]}." insert-last-word
  bindkey "${key_info[Escape]}_" insert-last-word

  # <Ctrl-x><Ctrl-e> to edit command-line in EDITOR
  autoload -Uz edit-command-line && zle -N edit-command-line && \
      bindkey "${key_info[Control]}x${key_info[Control]}e" edit-command-line

  # Bind <Shift-Tab> to go to the previous menu item.
  if [[ -n ${key_info[BackTab]} ]] bindkey ${key_info[BackTab]} reverse-menu-complete

  # Use smart URL pasting and escaping.
  autoload -Uz bracketed-paste-url-magic && zle -N bracketed-paste bracketed-paste-url-magic
  autoload -Uz url-quote-magic && zle -N self-insert url-quote-magic

  if zstyle -t ':zim:input' double-dot-expand; then
    double-dot-expand() {
      # Expand .. at the beginning, after space, or after any of ! " & ' / ; < > |
      if [[ ${LBUFFER} == (|*[[:space:]!\"\&\'/\;\<\>|]).. ]]; then
        LBUFFER+=/..
      else
        LBUFFER+=.
      fi
    }
    zle -N double-dot-expand
    bindkey . double-dot-expand
    bindkey -M isearch . self-insert
  fi

  autoload -Uz is-at-least
  if ! is-at-least 5.3; then
    # Redisplay after completing, and avoid blank prompt after <Tab><Tab><Ctrl-C>
    expand-or-complete-with-redisplay() {
      print -Pn ...
      zle expand-or-complete
      zle redisplay
    }
    zle -N expand-or-complete-with-redisplay
    bindkey "${key_info[Control]}I" expand-or-complete-with-redisplay
  fi

  # if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
  #   # Enable application mode when zle is active
  #   start-application-mode() {
  #     echoti smkx
  #   }
  #   stop-application-mode() {
  #     echoti rmkx
  #   }

  #   if is-at-least 5.3; then
  #     autoload -Uz add-zle-hook-widget && \
  #         add-zle-hook-widget -Uz line-init start-application-mode && \
  #         add-zle-hook-widget -Uz line-finish stop-application-mode
  #   else
  #     zle -N zle-line-init start-application-mode
  #     zle -N zle-line-finish stop-application-mode
  #   fi
  # fi
}
