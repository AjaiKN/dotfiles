;;; lang/uiua/config.el -*- lexical-binding: t; -*-

(use-package! uiua-ts-mode
  :mode "\\.ua\\'"
  :config
  (add-hook 'uiua-ts-mode-local-vars-hook #'lsp! 'append)
  (when (modulep! :tools lsp -eglot)
    (require 'lsp-mode)
    (add-to-list 'lsp-language-id-configuration '(uiua-ts-mode . "uiua"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("uiua" "lsp"))
      :activation-fn (lsp-activate-on "uiua")
      :server-id 'uiua)))
  (when (modulep! :tools lsp +eglot)
    (set-eglot-client! 'uiua-ts-mode '("uiua" "lsp"))))
