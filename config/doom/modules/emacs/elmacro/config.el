;;; emacs/elmacro/config.el -*- lexical-binding: t; -*-

(require 'akn)
(require 'el-patch)

(defmacro +elmacro--simple-processor (matcher &rest body)
  (declare (indent defun))
  (cl-with-gensyms (commands cmd)
    `(lambda (,commands)
       (cl-loop for ,cmd in ,commands
                collect
                (pcase ,cmd
                  (,matcher ,@body)
                  (_ ,cmd))))))

(use-package! elmacro
  :defer t
  :config
  (setq! elmacro-unwanted-commands-regexps
         (list "^(ido.*)$" "^(smex)$"
               (regexp-opt (mapcar #'symbol-name akn/all-scroll-commands))
               (regexp-opt (list (temporary-file-directory)
                                 doom-cache-dir
                                 "flycheck_config.el"))
               (rx "handle-focus")
               ;; TODO: not sure why so many make-directory calls are generated
               (rx "make-directory")
               (rx "elmacro-clear-command-history")
               (rx "(execute-extended-command"))
         elmacro-additional-recorded-functions
         (list #'copy-file
               #'copy-directory
               #'rename-file
               #'delete-file
               #'make-directory)
         elmacro-processors
         (list #'elmacro-processor-filter-unwanted
               #'elmacro-processor-prettify-inserts
               #'elmacro-processor-concatenate-inserts
               #'akn/deep-unpropertize-strings
               (+elmacro--simple-processor
                 `(akn/evil-backward-char-cross-lines . ,args)
                 `(evil-backward-char . ,args))
               (+elmacro--simple-processor
                 `(akn/evil-forward-char-cross-lines . ,args)
                 `(evil-forward-char . ,args))
               (+elmacro--simple-processor
                 `(akn/elisp-lookup-documentation . ,args)
                 `(+lookup/documentation . ,args))
               (+elmacro--simple-processor
                 `(,(and fn (or 'evil-forward-char 'evil-backward-char)) ,a ,b nil)
                 `(,fn ,a ,b))
               (+elmacro--simple-processor
                 `(,(and fn (or 'evil-forward-char 'evil-backward-char)) ,a nil)
                 `(,fn ,a))
               (+elmacro--simple-processor
                 `(evil-forward-char ,count t)
                 `(forward-char ,count))
               (+elmacro--simple-processor
                 `(evil-backward-char ,count t)
                 `(backward-char ,count))
               (+elmacro--simple-processor
                 `(,(and fn (or 'forward-char 'backward-char 'evil-forward-char 'evil-backward-char 'evil-previous-line 'evil-next-line)) nil)
                 `(,fn 1))
               (lambda (commands)
                 (let (ret)
                   (dolist (cmd commands)
                     (pcase cmd
                       (`(,(and fn (or 'forward-char 'evil-forward-char 'evil-backward-char 'evil-previous-line 'evil-next-line)) ,num)
                        (if (eq (caar ret) fn)
                            (cl-incf (cadar ret) num)
                          (push `(,fn ,num) ret)))
                       (_
                        (push cmd ret))))
                   (nreverse ret)))
               (lambda (commands)
                 (let (ret prev-cmd)
                   (dolist (cmd commands)
                     (cond
                      ((and (equal cmd prev-cmd) (eq (caar ret) #'dotimes))
                       (cl-incf (caadar ret)))
                      ((equal cmd prev-cmd)
                       (setf (car ret) (copy-tree `(dotimes (2) ,cmd))))
                      (t
                       (push cmd ret)))
                     (setq prev-cmd cmd))
                   (nreverse ret)))))

  :config/el-patch
  (el-patch-defun elmacro-record-command (advised-function function &optional record keys)
    "Advice for `call-interactively' which makes it temporarily record
commands in variable `command-history'."
    (let ((original-record record)
          retval)
      (elmacro-debug-message "[%s] ----- START -----" function)
      (setq record (or original-record (not (minibufferp)))) ;; don't record when in minibuffer
      (elmacro-debug-message "[%s] before - history %s record %s original %s"
                             function (car command-history) record original-record)
      (setq retval (funcall advised-function function record keys))
      (elmacro-debug-message "[%s] after - history %s" function (car command-history))
      (let* ((sexp (car command-history))
             (cmd (car sexp)))
        (when record
          (elmacro-debug-message "[%s] recording %s" function cmd)
          (when (or (eq cmd 'self-insert-command)
                    (el-patch-swap
                      (command-remapping 'self-insert-command)
                      (and (not (and (bound-and-true-p evil-local-mode)
                                     (memq evil-state '(normal motion visual))))
                           (command-remapping 'self-insert-command))))
            (!cons (elmacro-setq-last-command-event) elmacro-command-history))
          (!cons sexp elmacro-command-history)
          (!cdr command-history)
          (elmacro-debug-message "[%s] clean %s" function (car command-history)))
        (elmacro-debug-message "[%s] ----- STOP -----" function)
        retval))))
