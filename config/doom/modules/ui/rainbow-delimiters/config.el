;;; ui/rainbow-delimiters/config.el -*- lexical-binding: t; -*-

(add-hook! '(prog-mode-hook ;conf-mode-hook
             ;; after applying styles to TeX buffers
             TeX-update-style-hook)
             ;; c-mode-common-hook
             ;; clojure-mode-hook
             ;; lisp-mode-hook
             ;; csharp-mode-hook
             ;; dart-mode-hook
             ;; emacs-lisp-mode-hook lisp-data-mode-local-vars-hook
             ;; graphql-mode-hook
             ;; java-mode-hook
             ;; rjsx-mode
             ;; typescript-mode
             ;; typescript-tsx-mode
             ;; fennel-mode-hook
             ;; php-mode-hook
             ;; purescript-mode-hook
             ;; racket-mode-hook racket-hash-lang-mode-hook
             ;; rustic-mode-hook
             ;; scheme-mode-hook
             ;; sh-mode-hook
             ;; zig-mode-hook)
           #'rainbow-delimiters-mode)

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp. I reduce it from it's default of 9 to reduce the
;; complexity of the font-lock keyword and hopefully buy us a few ms of
;; performance.
(setq rainbow-delimiters-max-face-count 4)
