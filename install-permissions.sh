#!/bin/sh

set -x
cd "$HOME" || exit 1

# chmod go-rwx "$HOME"
chmod go-rwx prog 2>/dev/null
chmod go-rwx .ssh .gnupg 2>/dev/null
chmod go-rwx Trash .Trash "${XDG_DATA_HOME:-.local/share}"/Trash .local/share/Trash 2>/dev/null
chmod go-rwx Desktop Documents Downloads Finance Library Movies Music Pictures 2>/dev/null
chmod go-rwx work projects project github 2>/dev/null
chmod go-rwx ./*.gpg .*.gpg 2>/dev/null
chmod go-rwx ./*history .*history 2>/dev/null
chmod go-rwx Zotero ZotMoov 2>/dev/null
chmod go-rwx .config .local .local/bin .local/share .local/state .cache 2>/dev/null
chmod go-rwx "${XDG_CONFIG_HOME:-.cache}" "${XDG_STATE_HOME:-.local/state}" "${XDG_DATA_HOME:-.local/share}" "${XDG_CACHE_HOME:-.cache}" 2>/dev/null
chmod go-rwx ./*rc .*rc 2>/dev/null

chmod go-rwx "$DOTFILES"
