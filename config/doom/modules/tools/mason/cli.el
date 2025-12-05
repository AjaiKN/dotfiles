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
      (print! "hi")
      (load! "./autoload.el")
      (mason-ensure (lambda () (setq +mason--ensured t)))
      (while (not +mason--ensured)
        (sit-for 0.5))
      (print! (start "Installing LSPs..."))
      (print-group!
        (+mason/install-all-lsps)
        (while +mason--packages-pending
          (sit-for 0.5)))
      (print! "Installing LSPs...done"))))
