sh-base-mode sh-mode
:when (cl-loop for shell = sh-shell then (alist-get shell sh-ancestor-alist)
               while shell
               thereis (eq shell 'sh))

(cdhere "cd \"$(dirname \"\$0\")\"")
(safemode & "set -euo pipefail" n
          "# shellcheck disable=SC2154" n
          "trap 's=$?; echo >&2 \"$0: Error on line \"$LINENO\": $BASH_COMMAND\"; exit $s' ERR" n)
(verbose "set -x")
(__bash (if (bobp) "#!/usr/bin/env bash\n\n")
        (i safemode)
        (i verbose) n n)

(alias "alias "p"=\""r"\"")

(if "if " p "; then" n> r n "fi" > %)
(case "case " p " in" n> r n "esac" > %)
(while "while " p "; do" n> r n "done" > %)
(until "until " p "; do" n> r n "done" > %)
(ife "if " p "; then" n> p n "else" > n> q n> "fi" > %)
(for "for "p" in "p"; do" n> r n "done" > %)

(switch (i case))

(function "function " p " {" n> r n "}" > %)
(fn p"() {" n> r n "}" > %)

(parseargs
 (indent
  "print_usage() {
    echo \"Usage: $0 \"
    echo
    echo \"  -f, --force     force\"
    echo \"  -n, --dry-run   do not actually do anything; just say what would be done\"
    echo \"  -v, --verbose   be more verbose\"
  }

  force=
  dry_run=
  verbose=
  while [ $# -gt 0 ]; do
    case \"$1\" in
      --version)
        echo \""(p (file-name-base (or (buffer-file-name) (buffer-name) "Program")))" 0.0.1-pre\"
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
  done" n))
