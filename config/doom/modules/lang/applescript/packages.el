;;; lang/applescript/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; (package! applescript-mode :pin "82e5c35d0de9c8db6281aed21105f09acbb69eba")

(package! apples-mode
  :recipe (:host github
           :repo "AjaiKN/apples-mode"
           :local-repo "~/prog/emacs/apples-mode"))
