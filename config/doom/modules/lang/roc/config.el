;;; lang/roc/config.el -*- lexical-binding: t; -*-
;; General
(use-package! roc-ts-mode
  :defer t
  :mode ("\\.roc-ts\\'" . roc-ts-mode)
  :config
  (map! :map roc-ts-mode-map
        (:localleader
         "f" #'roc-ts-format
         "b" #'roc-ts-build
         "t" #'roc-ts-test
         "r" #'roc-ts-run
         "d" #'roc-ts-dev
         "c" #'roc-ts-check
         "e" #'roc-ts-repl
         (:prefix ("s" . "roc-start")
                  "f" #'roc-ts-start-fetch
                  "a" #'roc-ts-start-app
                  "p" #'roc-ts-start-pkg
                  "u" #'roc-ts-start-update)))

  (set-popup-rule! (rx bol "*roc-ts-repl") :size 0.3 :quit #'akn/insert-state-and-close-popup-h)

  ;; Setup the LSP support:
  ;; For this to work, you'll need roc_language_server, which is distributed in
  ;; Roc releases, in your PATH.
  (when (modulep! :tools lsp -eglot)
    (require 'lsp-mode)
    (add-to-list 'lsp-language-id-configuration '(roc-ts-mode . "roc"))
    (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_language_server")
                                          :activation-fn (lsp-activate-on "roc")
                                          :major-modes '(roc-ts-mode)
                                          :server-id 'roc_ls)))
  (when (modulep! :tools lsp +eglot)
    (set-eglot-client! 'roc-ts-mode '("roc_language_server")))
  (add-hook 'roc-ts-mode-local-vars-hook #'lsp!)

  ;; Formatting
  (set-formatter! 'roc-ts-format '("roc" "format" "--stdin" "--stdout") :modes '(roc-ts-mode))
  (setq-hook! 'roc-ts-mode-local-vars-hook
    +format-with 'roc-ts-format)

  ;; Keywords that should trigger electric indentation
  (set-electric! 'roc-ts-mode :words '("else"))

  ;; smartparens
  (after! smartparens
    (sp-with-modes 'roc-ts-mode
      ;; Since lambdas use a backslash, typing \{ should not auto-insert \}
      (sp-local-pair "\\{" nil :actions nil)
      (sp-local-pair "\\(" nil :actions nil)))

  ;; Extra ligatures
  (when (modulep! :ui ligatures +extra)
    (set-ligatures! 'roc-ts-mode
      :true "Bool.true" :false "Bool.false"
      :not "!"
      :and "&&" :or "||")
    (setq-hook! 'roc-ts-mode-hook
      prettify-symbols-compose-predicate #'+roc-ts-symbol-compose-p)
    (defun +roc-ts-symbol-compose-p (start end match)
      "Like `prettify-symbols-default-compose-p', except that if the
match is !, it checks that it's a logical NOT rather than the !
suffix operator (syntactic sugar for Task.await; see URL
  `https://www.roc-lang.org/tutorial#the-!-suffix')."
      (and (prettify-symbols-default-compose-p start end match)
           (or (not (equal match "!"))
               (and
                ;; character before isn't a word character
                (not (eq (char-syntax (char-before start))
                         ?w))
                ;; character after is a word character or open paren
                (memq (char-syntax (char-after end))
                      '(?\( ?w))))))))

;; Personal
(after! roc-ts-mode
  ;; https://gitlab.com/tad-lispy/nixos-configuration/-/blob/main/doom-emacs/modules/lang/roc/config.el
  ;; TODO: Remove once I'm done working on the highlighting.
  ;; NOTE: for me, adding this seems to crash emacs
  ;; (add-hook! 'roc-ts-mode-hook #'treesit-inspect-mode)

  ;; not sure why this wasn't on already
  (add-hook 'roc-ts-mode-hook #'akn/shift-select-mode))
