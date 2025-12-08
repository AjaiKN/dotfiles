;;; tools/mason/cli.el -*- lexical-binding: t; -*-

;; Autoloads don't work in the CLI.
(load! "./autoload.el")

(defcustom +mason-log-file (expand-file-name "logs/mason.log" doom-state-dir)
  "The file where Mason logs are sent when installing packages with the CLI.

This holds the content of the `mason-buffer'."
  :group '+mason
  :type 'file)

(defvar +mason--ensured nil)
(defvar +mason--updated nil)

(defun +mason--sync (&optional reinstall?)
  "Synchronously install all needed Mason packages."
  (print! (start "Installing LSPs..."))
  (mkdir (file-name-directory +mason-log-file) 'parents)
  (with-temp-buffer (write-file +mason-log-file))
  (add-hook 'kill-emacs-hook #'+mason--abort-warning-h 1)
  (print-group!
    (print! (item "Loading Mason...\e[1A"))
    (mason-ensure (lambda () (setq +mason--ensured t)))
    ;; Wait until Mason is ensured.
    (while (not +mason--ensured)
      (sit-for 0.05))
    (print! (success "\rLoading Mason...done"))

    (when reinstall?
      (print! (item "Updating Mason registry...\e[1A"))
      (mason-update-registry (lambda () (setq +mason--updated t)))
      (while (not +mason--updated)
        (sit-for 0.05))
      (print! (success "\rUpdating Mason registry...done")))

    (+mason/install-all-lsps reinstall?)
    ;; Wait until Mason finishes installing the LSPs.
    (while +mason--packages-pending
      (sit-for 0.05)
      (when (get-buffer mason-buffer)
        (with-current-buffer mason-buffer
          (write-file +mason-log-file))))
    (remove-hook 'kill-emacs-hook #'+mason--abort-warning-h)
    (when +mason--packages-failed
      (print! (warn "Failed to install some LSPs: %s") +mason--packages-failed)
      (print! (warn "Mason log file: %s") (path +mason-log-file))
      (exit! 68))))

(defun +mason--abort-warning-h ()
  (dlet ((doom-print-indent 6))
    (print! (warn "Mason log file: %s") (path +mason-log-file))))

(defcli! ((sync s))
    ((reinstall? ("-u" "--update" "-f" "--force" "--reinstall") "Reinstall all Mason packages"))
  "Install all Mason packages that correspond to enabled language modules."
  (doom-packages-ensure)
  (+mason--sync reinstall?))

;;;; +sync flag

(defvar +mason--need-update nil)

;; Keep track of whether the -u flag for doom sync is enabled.
(define-advice doom-packages-update (:before (&optional pinned-only-p &rest _) +mason--sync-on-update)
  (unless pinned-only-p (setq +mason--need-update t)))

(add-hook! 'doom-after-sync-hook
  (defun +mason--install-h ()
    (when (modulep! :tools mason +sync)
      (+mason--sync +mason--need-update))))
