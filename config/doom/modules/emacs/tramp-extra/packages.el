;; -*- no-byte-compile: t; -*-
;;; emacs/tramp-extra/packages.el

(when (modulep! +hlo)
  (package! tramp-hlo :pin "703c2b0d83a486d3010774eaf97b259b82d574f0"
    :recipe (:host github :repo "jsadusk/tramp-hlo")))
