;;; term/eshell-extra/autoload/howardism.el -*- lexical-binding: t; -*-

;; https://howardism.org/Technical/Emacs/eshell-why.html
;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org (CC0)

(require 'dash)
(require 's)
(require 'seq)
(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun eshell/set (&rest args)
  "Creates a buffer local variable.
The first parameters of ARGS is the name of the variable.
The other parameters are the values. If not given, the
variable is deleted."
  (let* ((var (car args))
         (var-sym (make-symbol var))
         ;; Convert value to a string
         (val (pcase (seq-length (cdr args))
                (0 nil)
                (1 (format "%s" (cadr args)))
                (_ (thread-last (cdr args)
                                (seq-map 'eshell-stringify)
                                (s-join " "))))))
    (if val
        (progn
          (set (make-local-variable var-sym) val)
          (setenv var val))

      ;; If we don't get both a variable and a value, let's try to
      ;; delete the variable:
      (makunbound var-sym)
      (setenv var))))

;;;###autoload
(defun eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (+eshell-fn-on-files 'view-file 'view-file-other-window files))

;;;###autoload
(defalias 'eshell/more 'eshell/less)

;;;###autoload
(defun eshell/do (&rest args)
  "Execute commands over lst.

Example: do chmod -x :: *.csv(x)"
  (seq-let (cmd lst) (-split-on "::" args)
    (dolist (file (flatten-list (append lst)))
      (cl-pushnew file cmd)
      (eshell-named-command (car cmd) (cdr cmd)))))
