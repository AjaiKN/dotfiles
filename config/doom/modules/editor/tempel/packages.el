;; -*- no-byte-compile: t; -*-
;;; editor/tempel/packages.el

(package! tempel :pin "7120539bf047d3748636a7299fd7bca62ab2c74a")

(when (modulep! :completion vertico)
  (package! consult-tempel
    :recipe (:host github :repo "AjaiKN/consult-tempel" :local-repo "~/prog/emacs/consult-tempel")))

;; Possible other packages
;; tempel-collection
;; eglot-tempel
;; https://github.com/svaante/lsp-snippet
