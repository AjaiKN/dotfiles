;; -*- no-byte-compile: t; -*-
;;; emacs/casual/packages.el

(package! casual :pin "0c0a386bb9e73532679a29a270cfdd2c6e386d66")
(package! casual-avy :pin "c5bc8e9d57a843f75e6125f097550414af3d5ec7")
(when (modulep! :ui highlight-symbol)
  (package! casual-symbol-overlay :pin "1453e7486dd0921f0319f21dd8c8b603e4eb7300"))
