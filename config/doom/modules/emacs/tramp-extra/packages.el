;; -*- no-byte-compile: t; -*-
;;; emacs/tramp-extra/packages.el

(when (modulep! +hlo)
  (package! tramp-hlo :pin "c74cb27b978e36939da26aa5f4f7a85d9ae3ab39"
    :recipe (:host github :repo "jsadusk/tramp-hlo")))
