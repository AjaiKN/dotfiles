;;; tools/mason/autoload.el -*- lexical-binding: t; -*-

(require 'mason)

(defvar +mason--ensured nil)

(defvar +mason--packages-pending nil)
(defvar +mason--packages-failed nil)

;;;###autodef
(defmacro mason-after-ensured! (&rest body)
  (declare (indent 0))
  `(if +mason--ensured
       ,(macroexp-progn body)
     (mason-ensure
      (lambda ()
        (setq +mason--ensured t)
        ,@body))))

(defun +mason--print-progress ()
  (when +mason--packages-pending
    (princ "\r")
    (print! (item "Installing %d Mason packages: %s\e[1A")
            (length +mason--packages-pending)
            (truncate (string-trim (format "%s" +mason--packages-pending) "(" ")")))))

;;;###autodef
(defun mason-install! (package &optional suffix)
  (push package +mason--packages-pending)
  (mason-after-ensured!
    (ignore-errors ;with-demoted-errors "Mason error: %S"
      (mason-install package
                     'force nil
                     (lambda (success)
                       (if success
                           (print! (success "\rInstalled %s%s") package (or suffix ""))
                         (print! (error "\rFailed to install %s%s!") package (or suffix ""))
                         (push package +mason--packages-failed))
                       (when (not success)
                         (warn "Couldn't install %s%s" package (or suffix "")))
                       (setq +mason--packages-pending (delete package +mason--packages-pending))
                       (+mason--print-progress))))))

;;;###autoload
(defun +mason/install-all-lsps (&optional reinstall)
  (interactive)
  (when (modulep! :tools lsp)
    (mason-after-ensured!
      (pcase-dolist (`(,language . ,packages) +mason-lsp-programs)
        (let ((suffix (format " (:lang %s)" language)))
          (dolist (package packages)
            (if (and (not reinstall)
                     (file-exists-p! (file-name-concat mason-dir "packages" package))
                     (not (directory-empty-p (file-name-concat mason-dir "packages" package))))
                (print! (success "%s already installed%s") package suffix)
              (when (eval `(modulep! :lang ,language +lsp) t)
                (mason-install! package suffix))))))
      (+mason--print-progress))))
