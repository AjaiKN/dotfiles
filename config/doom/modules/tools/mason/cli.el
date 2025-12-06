;;; tools/mason/cli.el -*- lexical-binding: t; -*-

(defvar +mason-log-file (expand-file-name "logs/mason.log" doom-state-dir))
(defvar +mason--need-update nil)

(load! "./init.el")

(defun +mason--abort-warning-h ()
  (dlet ((doom-print-indent 6))
    (print! (warn "Mason log file: %s") (path +mason-log-file))))

(defun +mason--sync (&optional reinstall?)
  (load! "./autoload.el")
  (print! (start "Installing LSPs..."))
  (mkdir (file-name-directory +mason-log-file) 'parents)
  (with-temp-buffer (write-file +mason-log-file))
  (add-hook 'kill-emacs-hook #'+mason--abort-warning-h 1)
  (print-group!
    (print! (item "Loading Mason...\e[1A"))
    (mason-ensure (lambda () (setq +mason--ensured t)))
    (while (not +mason--ensured)
      (sit-for 0.05))
    (print! (success "\rLoading Mason...done"))
    (+mason/install-all-lsps reinstall?)
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

;; (defcli! ((sync s))
;;     ((reinstall? ("-u" "--update" "-f" "--force" "--reinstall") "Reinstall all Mason packages"))
;;   "Install all Mason packages that correspond to enabled language modules."
;;   (+mason--sync reinstall?))

(define-advice doom-packages-update (:before (&optional pinned-only-p &rest _) +mason--sync-on-update)
  (unless pinned-only-p (setq +mason--need-update t)))

(add-hook! 'doom-after-sync-hook
  (defun +mason--install-h ()
    (when (modulep! :tools mason +sync)
      (+mason--sync +mason--need-update))))
