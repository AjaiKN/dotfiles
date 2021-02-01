;; -*- no-byte-compile: t; -*-
;;; term/eat/packages.el

(package! eat
  :pin "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"
  :recipe (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
