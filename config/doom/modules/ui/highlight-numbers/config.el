;;; ui/highlight-numbers/config.el -*- lexical-binding: t; -*-

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp
        (rx symbol-start
            (+ digit)
            (? "." (* (in "0-9")))
            symbol-end))

  (puthash 'elixir-mode
           "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
           highlight-numbers-modelist))

(after! so-long
  (pushnew! so-long-minor-modes 'highlight-numbers-mode))

(after! ielm
  (require 'highlight-numbers)
  (pushnew! ielm-font-lock-keywords
            (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode)))
