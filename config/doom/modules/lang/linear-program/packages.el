;; -*- no-byte-compile: t; -*-
;;; lang/linear-program/packages.el

(package! linear-program-mode
  :recipe (:local-repo "~/.config/doom/lisp/linear-program-mode"
           :build (:not compile)))

(package! minizinc-ts-mode)
