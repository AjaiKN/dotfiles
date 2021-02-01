;;; cli.el -*- lexical-binding: t; -*-

(load! "~/.config/emacs/lisp/cli/upgrade")

(defmacro akn/cli-fail! (msg &rest args)
  `(progn
     (print! ,msg ,@args)
     (exit! 1)))

(defcli! ((upgrade-doom u))
    ((yes?       ("-y" "--yes") "Don't ask whether to proceed with the upgrade")
     (aot?       ("--aot") "Natively compile packages ahead-of-time (if available)")
     (jobs       ("-j" "--jobs" num) "How many CPUs to use for native compilation")
     (nobuild?   ("-B") "Don't rebuild packages when hostname or Emacs version has changed")
     (fail-if-up-to-date? ("--fail-if-up-to-date") "Return non-zero exit code if already up-to-date")
     &context context)
  "Updates Doom's core only without upgrading packages.

A convenience command for updating Doom's core and pinned modules/module
libraries. It is the equivalent of the following shell commands:

    $ cd ~/.config/emacs
    $ git pull --rebase
    $ doom sync"
  (let* ((yes? (or yes? (doom-cli-context-suppress-prompts-p context)))
         (sync-cmd (append '("sync")
                           (if aot? '("--aot"))
                           (if nobuild? '("-B"))
                           (if jobs `("-j" ,jobs)))))
    (akn/fetch-doom)
    (cond
     ((doom-cli-upgrade context yes? nil)
      (print! (item "Reloading Doom Emacs"))
      (doom-cli-context-put context 'upgrading t)
      (call! sync-cmd)
      (print! (success "Finished upgrading Doom Emacs")))

     ((print! "Doom is up-to-date!")
      (call! sync-cmd)
      (when fail-if-up-to-date?
        ;; exit codes 16-192 are "reserved for the user's extensions"
        (exit! 90))))))

(defcli! ((check-for-updates))
    ()
  "Check if Doom's core has any updates available.

If we're up-to-date already, the exit code is 0.
If updates are available, the exit code is 100.
If we're out of sync, the exit code is 110."
  (akn/fetch-doom)
  (let* ((default-directory doom-emacs-dir)
         process-file-side-effects
         (this-rev (cdr (sh! "git" "rev-parse" "HEAD")))
         (new-rev  (cdr (sh! "git" "rev-parse" "upstream")))
         (is-ancestor (= (car (sh! "git" "merge-base" "--is-ancestor" this-rev new-rev)) 0)))
    (cond
     ((equal this-rev new-rev)
      (print! "Doom is up-to-date! (%s)" this-rev))
     ((not is-ancestor)
      (print! "Doom is out of sync with upstream. (%s -> %s)" this-rev new-rev)
      (print! "The current commit, %s, is not an ancestor of the upstream commit, %s." this-rev new-rev)
      (print! "So we won't be able to fast-forward.")
      (exit! 110))
     (t
      (print! "Doom is out of date. (%s -> %s)" this-rev new-rev)
      (exit! 100)))))

(defun akn/fetch-doom ()
  (let ((default-directory doom-emacs-dir)
        process-file-side-effects)
    ;; (print! (start "Fetching upstream remote..."))
    ;; fails if the remote already exists; that's fine
    (sh! "git" "remote" "add" "upstream" doom-upgrade-url)
    (or (zerop (car (sh! "git" "remote" "set-url" "upstream" doom-upgrade-url)))
        (akn/cli-fail! "Failed to set-url upstream"))
    (or (zerop (car (sh! "git" "config" "branch.master.remote" "upstream")))
        (akn/cli-fail! "Failed to set master.remote to upstream"))
    (or (zerop (car (sh! "git" "config" "branch.master.merge" "refs/heads/master")))
        (akn/cli-fail! "Failed to set master.merge to refs/heads/master"))
    (or (zerop (car (sh! "git" "fetch" "--tags" "upstream")))
        (akn/cli-fail! "Failed to fetch from upstream"))))
