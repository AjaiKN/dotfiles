#License for this file:
# MIT License
#
# Copyright (c) 2009-2011 Robby Russell and contributors
# Copyright (c) 2011-2015 Sorin Ionescu and contributors
# Copyright (c) 2015-2016 Matt Hamilton and contributors
# Copyright (c) 2020 Roman Perepelitsa
# Copyright (c) 2016-2025 Eric Nielsen, Matt Hamilton and contributors
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

# https://github.com/umlx5h/zsh-manpage-completion-generator
# This should be at the end of fpath.
fpath+=($HOME/.local/share/zsh/generated_man_completions)
# make sure my stuff is at the front
add_to_fpath "$ZSH_CACHE_DIR/completions" "$ZSH_CUSTOM/completions" "$ZSH_CUSTOM/functions"

autoload -Uz is-at-least
# from https://github.com/zimfw/completion/blob/master/init.zsh
if (( ${+_comps} )); then
  # https://github.com/zimfw/zimfw/wiki/Troubleshooting#completion-is-not-working
  print -u2 'warning: completion was already initialized before completion module. Will call compinit again.'
fi

# not using -C because https://github.com/romkatv/zsh-bench?tab=readme-ov-file#cutting-corners
# autoload -Uz compinit
# compinit -d "$ZSH_CACHE_DIR/zcompdump"

# from https://github.com/zimfw/completion/blob/master/init.zsh
() {
  builtin emulate -L zsh -o EXTENDED_GLOB

  local zdumpfile="$ZSH_CACHE_DIR/zcompdump"

  # the extended glob we need doesn't seem to be supported in earlier zsh versions
  if is-at-least 5.2; then
    # Check if dumpfile is up-to-date by comparing the full path and
    # last modification time of all the completion functions in fpath.
    local zold_dat
    local -a zmtimes
    local -i zdump_dat=1
    LC_ALL=C local -r zcomps=(${^fpath}/^([^_]*|*~|*.zwc)(N))
    if (( ${#zcomps} )); then
      zmodload -F zsh/stat b:zstat && zstat -A zmtimes +mtime ${zcomps} || return 1
    fi
    local -r znew_dat=${ZSH_VERSION}${ZSH_PATCHLEVEL}$'\0'${(pj:\0:)zcomps}$'\0'${(pj:\0:)zmtimes}
    if [[ -e ${zdumpfile}.dat ]]; then
      zmodload -F zsh/system b:sysread && sysread -s ${#znew_dat} zold_dat <${zdumpfile}.dat || return 1
      if [[ ${zold_dat} == ${znew_dat} ]] zdump_dat=0
    fi
    if (( zdump_dat )); then
      # echo removing
      command rm -f ${zdumpfile}(|.dat|.zwc(|.old))(N) || return 1
    fi

    # Load and initialize the completion system
    autoload -Uz compinit && compinit -C -d ${zdumpfile} && [[ -e ${zdumpfile} ]] || return 1

    if [[ ! ${zdumpfile}.dat -nt ${zdumpfile} ]]; then
      >! ${zdumpfile}.dat <<<${znew_dat}
    fi
    # Compile the completion dumpfile; significant speedup
    if [[ ! ${zdumpfile}.zwc -nt ${zdumpfile} ]]; then
      zcompile ${zdumpfile}
    fi
  else
    # command rm -f ${zdumpfile}(|.dat|.zwc(|.old))(N) || return 1
    autoload -Uz compinit && compinit -d ${zdumpfile} || return 1
  fi
}

# from https://github.com/zimfw/completion/blob/master/init.zsh
functions[compinit]=$'print -u2 \'warning: compinit being called again after completion module at \'${funcfiletrace[1]}
'${functions[compinit]}
