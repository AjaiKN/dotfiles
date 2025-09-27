;;; completion/hippie/config.el -*- lexical-binding: t; -*-

;; https://tecosaur.github.io/emacs-config/#hippie-expand
(after! hippie-exp
  (setq hippie-expand-try-functions-list
        `(,@(when (modulep! :editor snippets)
              (autoload #'yas-hippie-try-expand "yasnippet")
              '(yas-hippie-try-expand))
          try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-all-abbrevs
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev-from-kill
          try-expand-list
          try-expand-whole-kill
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  ;; Use the existing dabbrev settings for ignoring buffers for hippie-expand.
  (add-transient-hook! #'hippie-expand
    (require 'dabbrev)
    (setq hippie-expand-ignore-buffers
          (seq-uniq (append hippie-expand-ignore-buffers
                            dabbrev-ignored-buffer-modes
                            (list (regexp-opt dabbrev-ignored-buffer-names))
                            dabbrev-ignored-buffer-regexps))))

  ;; https://tecosaur.github.io/emacs-config/#suffix-stripping
  (advice-add #'he-substitute-string :filter-args #'+hippie--he-substitute-string-a))
