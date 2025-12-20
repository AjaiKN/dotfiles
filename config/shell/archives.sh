# shellcheck shell=bash

# also see ohmyzsh `takeurl` (or just `take`)

# Usage:
#   untar/unarchive FILE [OPTIONS...]
#   mktar,tar-create,archive,archive-create DESTINATION [OPTIONS...] SOURCEFILES
#   tar-ls,archive-ls FILE [OPTIONS...]
#
# bsdtar is better when available.
# - bsdtar "can extract from tar, pax, cpio, zip, jar, ar, xar, rpm, 7-zip, and
#   ISO 9660 cdrom images and can create tar, pax, cpio, ar, zip, 7-zip, and
#   shar archives."
# - bsdtar has a good default when creating archives: restricted pax format,
#   "which will create ustar archives except for entries that require pax
#   extensions (for long filenames, ACLs, etc)."
#
# shellcheck disable=SC2139
if command -v bsdtar >/dev/null 2>&1; then
	alias tar=bsdtar
	alias {untar,unarchive}='bsdtar -xf'
	alias {mktar,tar-create,archive,archive-create}='bsdtar -caf'
	alias {tar-ls,archive-ls}='bsdtar -tvf'
else
	# e[x]tract [f]ile
	alias {untar,unarchive}='tar -xf'
	# [c]reate a [f]ile, with format determined [a]utomatically from destination file extension
	# (GNU tar uses the gnu format by default, so make sure it uses the more compatible and more flexible pax format)
	alias {mktar,tar-create,archive,archive-create}='tar -c --format=pax -af'
	# list files in archive
	alias {tar-ls,archive-ls}='tar -tvf'
fi

if command -v wget >/dev/null 2>&1; then
	alias download='wget'
else
	alias download='curl -O -L'
fi

function download-extract {
	local COLOR='\033[1;36m'
	local NOCOLOR='\033[0m'
	local cmd=tar
	if [[ $1 =~ .(tar|pax|cpio|zip|jar|ar|xar|pkg|xip|rpm|7z|iso|iso9660)$ ]]; then
		# bsdtar "can extract from tar, pax, cpio, zip, jar, ar, xar, rpm, 7-zip, and ISO 9660 cdrom images"
		cmd=bsdtar
	fi
	local outdir=${2:-output}
	if command -v "$cmd" >/dev/null 2>&1; then
		# first argument: URL
		# second argument: directory to put it in (to avoid tar-bombing)
		mkdir "$outdir" || return 1
		curl -L "$1" | "$cmd" xf - -C "$outdir" || return 1
		echo -e "Extracted into $COLOR$(realpath "$outdir")/$NOCOLOR ($(du -sh "$outdir" | cut -f -1)):"
		ls -Al "$outdir"
	else
		echo "command $cmd not found"
		return 1
	fi
}
