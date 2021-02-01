;;; lang/pdf-raw/config.el -*- lexical-binding: t; -*-

(use-package! pdf-mode
  :commands (pdf-mode)
  :config
  (add-hook! 'pdf-mode-hook
             #'pdf-fontify-buffer))

(after! pdf-view
  (add-hook! 'pdf-view-mode-hook
    (defun akn/on-mode-exit-turn-off-revert-buffer-function-h ()
      (add-hook! 'change-major-mode-hook :local
        (kill-local-variable 'revert-buffer-function)))))
