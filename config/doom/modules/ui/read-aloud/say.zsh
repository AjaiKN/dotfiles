#!/usr/bin/env zsh

# https://stackoverflow.com/a/33769559
zmodload zsh/datetime

wordstart=$'\e[m'
wordend=$'\e[7m'
extra=$'\e(B'

# sed 's/)/\n)/g' |
say --interactive=smso --rate=220 --input-file=- "$@" |
	{
		counter=0
		IFS= read -r -d $'\r' line # first line in the output of `say --interactive` suppresses the cursor; discard this line
		while IFS= read -r -d $'\r' line; do
			# (( counter++ )) || continue
			word=${${line%$wordstart*}#*$wordend}
			# word=${word//\$extra/}
			# word=$(echo "$word" | sed 's/\x1b(B//g')
			word=${word//$extra/}
			printf '%s\n' "$word"
		done
	}
