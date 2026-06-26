;;; editor/tempel/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! tempel)
  ;; :recipe (:local-repo "~/prog/emacs/tempel"))

(when (modulep! :completion vertico)
  (package! consult-tempel
    :recipe (:host github :repo "AjaiKN/consult-tempel" :local-repo "~/prog/emacs/consult-tempel")))

;; Possible other packages
;; tempel-collection
;; eglot-tempel
;; https://github.com/svaante/lsp-snippet
