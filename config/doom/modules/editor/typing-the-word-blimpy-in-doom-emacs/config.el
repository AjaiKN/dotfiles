;;; editor/typing-the-word-blimpy-in-doom-emacs/config.el -*- lexical-binding: t; -*-

(defvar-local +typing-the-word-blimpy-in-doom-emacs--old-values nil)

(use-package! blimpy
  :defer-incrementally emacs custom window use-package-core
  :config
  (set-popup-rule! "\\`\\(?:^\\)\\*blimpy\\*"
    :side 'bottom
    :height 0.76
    :quit 'current)

  (add-hook! 'blimpy-before-typing-the-word-blimpy-in-emacs-hook :append
             #'+typing-the-word-blimpy-in-doom-emacs--switch-to-evil-insert-state-h)

  (add-hook! 'doom-escape-hook
    (when blimpy-mode
      (blimpy-mode -1)
      t))

  (when (modulep! :editor evil)
    (setq-hook! 'blimpy-before-typing-the-word-blimpy-in-emacs-hook
      +typing-the-word-blimpy-in-doom-emacs--the-original-evil-state-before-typing-the-word-blimpy evil-state)
    (advice-add #'blimpy--for-each-char :after
                #'+typing-the-word-blimpy-in-doom-emacs--switch-back-to-the-original-evil-state-a))

  (defadvice! +typing-the-word-blimpy-in-doom-emacs--allow-the-user-to-type-just-one-C-u-a (args)
    :filter-args #'blimpy-type-the-word-blimpy-in-emacs
    (let ((force (car-safe args)))
      `(,(if-let* ((num (car-safe force))
                   ((numberp num))
                   ((null (cadr args)))
                   ((null (cadr force)))
                   (number-of-C-u-s (round (log num 4))))
             (cons
              (expt 4
                    (pcase number-of-C-u-s
                      (1 2)
                      (2 1)
                      (n n)))
              ())
           (list force)))))

  (add-hook! 'blimpy-mode-hook
    (defun +typing-the-word-blimpy-in-doom-emacs--disable-modes-h ()
      (if blimpy-mode
          (setq +typing-the-word-blimpy-in-doom-emacs--old-values
                (buffer-local-set-state abbrev-mode nil
                                        electric-indent-mode nil
                                        ;; or could use (add-to-list 'evil-escape-inhibit-functions (lambda () blimpy-mode))
                                        evil-escape-inhibit t))
        (buffer-local-restore-state +typing-the-word-blimpy-in-doom-emacs--old-values)))))
