;;; app/github-explorer/config.el -*- lexical-binding: t; -*-

(defvar akn/github-explorer-file-mode-map (make-sparse-keymap))
(define-minor-mode akn/github-explorer-file-mode
  "A mode for files that were fetched by github-explorer."
  :group 'akn
  :lighter "GITHUB EXPLORER"
  (akn/mode-set akn/github-explorer-file-mode
    buffer-read-only t))
(when (modulep! :editor evil)
  (evil-make-intercept-map akn/github-explorer-file-mode-map))

(use-package! github-explorer
  :defer t
  :autoload (github-explorer)
  :config
  (when (modulep! :editor evil)
    (evil-make-overriding-map github-explorer-mode-map))
  (defun akn/github-explorer-file-p ()
    (and (not buffer-file-name)
         (string-match-p (regexp-quote (format "*%s:" github-explorer-name)) (buffer-name))))
  (defadvice! akn/github-explorer-file+enable-mode-a (&rest _)
    :after #'github-explorer-apply-auto-mode
    (when (akn/github-explorer-file-p)
      (akn/github-explorer-file-mode)))
  (map! :map akn/github-explorer-file-mode-map
        :mnie "q" #'quit-window))

;; disable make-local warnings (Doom already disables the other 3: see `+emacs-lisp-linter-warnings')

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
