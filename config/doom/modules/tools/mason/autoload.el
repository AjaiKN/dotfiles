;;; tools/mason/autoload.el -*- lexical-binding: t; -*-

(require 'mason)

(defvar +mason--ensured nil)

(defvar +mason--packages-pending nil)

;;;###autodef
(defmacro mason-after-ensured! (&rest body)
  (declare (indent 0))
  `(if +mason--ensured
       ,(macroexp-progn body)
     (mason-ensure
      (lambda ()
        (setq +mason--ensured t)
        ,@body))))

;;;###autodef
(defun mason-install! (package)
  (push package +mason--packages-pending)
  (mason-after-ensured!
    (print! (blue "Installing %s...") package)
    (ignore-errors ;with-demoted-errors "Mason error: %S"
      (mason-install package
                     'force nil
                     (lambda (success)
                       (if success
                           (print! (green "Installing %s...done" package))
                         (print! (red "Installing %s...failed!" package)))
                       (when (not success)
                         (warn "Couldn't install %s" package))
                       (setq +mason--packages-pending (delete package +mason--packages-pending))
                       (print! "remaining: %S" +mason--packages-pending))))))

;;;###autoload
(defun +mason/install-all-lsps (&optional reinstall)
  (interactive)
  (print! "installing all lsps")
  (mason-after-ensured!
    (pcase-dolist (`(,language . ,packages) +mason-lsp-programs)
      (dolist (package packages)
        (if (and (not reinstall) (file-exists-p! (file-name-concat mason-dir "packages" package)))
            (print! "%s already installed" package)
          (when (eval `(modulep! :lang ,language) t)
            (mason-install! package)))))))
