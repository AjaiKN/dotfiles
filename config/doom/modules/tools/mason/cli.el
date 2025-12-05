;;; tools/mason/cli.el -*- lexical-binding: t; -*-

(load! "./init.el")

(add-hook! 'doom-after-sync-hook
  (defun +mason--install-h ()
    (when (modulep! :tools mason +sync)
      ;; (setq! doom-print-stream
      ;;        (lambda (char)
      ;;          (with-temp-buffer
      ;;            (insert char)
      ;;            (append-to-file (point-min) (point-max) "/tmp/mymason.log"))))
      (load! "./autoload.el")
      (print! (start "Installing LSPs..."))
      (print-group!
        (print! (item "Loading Mason...\e[1A"))
        (mason-ensure (lambda () (setq +mason--ensured t)))
        (while (not +mason--ensured)
          (sit-for 0.1))
        (print! (success "\rLoading Mason...done"))
        (+mason/install-all-lsps)
        (while +mason--packages-pending
          (sit-for 0.1))
        ;; (with-current-buffer mason-buffer
        ;;   (let ((str (buffer-string)))
        ;;     (unless (string-empty-p str)
        ;;       (print-group!
        ;;         (print! "%s" str))))
        ;;   (let ((inhibit-read-only t))
        ;;     (delete-region (point-min) (point-max))))))
        (if +mason--packages-failed
            (print! (error "Failed to install some LSPs: %s") +mason--packages-failed))))))
