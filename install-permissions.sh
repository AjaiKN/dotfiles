#!/bin/sh

cd "$HOME" || exit 1

filter_output() {
	while IFS= read -r line; do
		case "$line" in
			*'retained as'*) ;;
			*'could not be accessed'*) ;;
			*'No such file or directory'*) ;;
			*) printf '%s\n' "$line" ;;
		esac
	done
}

chmod="chmod"

# -v isn't part of POSIX
has_verbose_option=
if "$chmod" --help 2>&1 | grep -E '\[-[^ ]*v[^ ]*\]| -v[, ]|^-v' >/dev/null; then
	has_verbose_option=1
fi

mychmod() {
	if [ -n "$has_verbose_option" ]; then
		"$chmod" -vv "$@" 2>/dev/null | filter_output
	else
		(
			set -x
			"$chmod" "$@" 2>&1
		) | filter_output
	fi
}

# mychmod go-rwx "$HOME"
mychmod go-rwx prog
mychmod go-rwx .ssh .gnupg
mychmod go-rwx Trash .Trash "${XDG_DATA_HOME:-.local/share}"/Trash .local/share/Trash
mychmod go-rwx Desktop Documents Downloads Finance Library Movies Music Pictures Vidoes
mychmod go-rwx "${XDG_DESKTOP_DIR:-Desktop}" "${XDG_DOCUMENTS_DIR:-Documents}" "${XDG_DOWNLOAD_DIR:-Downloads}" "${XDG_MUSIC_DIR:-Music}" "${XDG_PICTURES_DIR:-Pictures}" "${XDG_VIDEOS_DIR:-Videos}"
mychmod go-rwx work projects project github
mychmod go-rwx ./*.gpg .*.gpg
mychmod go-rwx ./*history .*history
mychmod go-rwx ./*history_backup .*history_backup
mychmod go-rwx Zotero ZotMoov
mychmod go-rwx .config .local .local/bin .local/share .local/state .cache
mychmod go-rwx "${XDG_CONFIG_HOME:-.cache}" "${XDG_STATE_HOME:-.local/state}" "${XDG_DATA_HOME:-.local/share}" "${XDG_CACHE_HOME:-.cache}"
mychmod go-rwx ./*rc .*rc

mychmod go-rwx "$DOTFILES"
