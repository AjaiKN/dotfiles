sh-mode

(cd_dirname "cd \"$(dirname \"\$0\")\"")
(safemode & "set -euo pipefail" n
          "# shellcheck disable=SC2154" n
          "trap 's=$?; echo >&2 \"$0: Error on line \"$LINENO\": $BASH_COMMAND\"; exit $s' ERR" n)
(verbose "set -x")
(__bash (if (bobp) "#!/usr/bin/env bash\n\n")
        (i safemode)
        (i verbose) n n)
