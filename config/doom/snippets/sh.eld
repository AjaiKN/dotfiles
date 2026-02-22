sh-base-mode sh-mode

(cdhere "cd \"$(dirname \"\$0\")\"")
(safemode & "set -euo pipefail" n
          "# shellcheck disable=SC2154" n
          "trap 's=$?; echo >&2 \"$0: Error on line \"$LINENO\": $BASH_COMMAND\"; exit $s' ERR" n)
(verbose "set -x")
(__bash (if (bobp) "#!/usr/bin/env bash\n\n")
        (i safemode)
        (i verbose) n n)

(if "if " p "; then" n> p n> "else" n> p n> "fi")
(case "case " p " in" n> p ")" n> p n> ";;" n> q n "esac")
(for "for " p " in ; do" n> q n "done")

(parseargs "print_usage() {
	echo \"Usage: $0 \"
	echo
	echo \"  -f, --force     be more forceful\"
	echo \"  -n, --dry-run   do not actually do anything; just say what would be done\"
	echo \"  -v, --verbose   be more verbose\"
}

force=
dry_run=
verbose=
while [ $# -gt 0 ]; do
	case \"$1\" in
		--version)
			echo \"emacs-restart 0.0.1-pre\"
			exit
			;;
		-h|--help)
			print_usage
			exit 0
			;;
		-f|--force)
			force=1
			shift
			;;
		-n|--dry-run)
			dry_run=1
			shift
			;;
		-v|--verbose)
			verbose=1
			shift
			;;
		--)
			shift
			break
			;;
		-[^-][^-]*) # expand multiple single-char options (e.g. -vf)
			opt=\"$1\"
			shift
			other=\"${opt#-?}\"
			set -- \"${opt%\"$other\"}\" \"-${other}\" \"$@\"
			;;
		-*)
			echo \"ERROR: Unknown option: $1\"
			print_usage
			exit 1
			;;
		*)
			break
			;;
	esac
done")
