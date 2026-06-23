;;; lang/linear-program/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! linear-program-mode
  :recipe (:local-repo "~/.config/doom/lisp/linear-program-mode"
           :build (:not compile)))

(package! minizinc-ts-mode)
