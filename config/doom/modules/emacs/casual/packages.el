;; -*- no-byte-compile: t; -*-
;;; emacs/casual/packages.el

(package! casual :pin "988f28bfd0564247dbb10d4343d2241f847190f6")
(package! casual-avy :pin "7e8f7703f4ab4886f27241664aa5e1510103f74e")
(when (modulep! :ui highlight-symbol)
  (package! casual-symbol-overlay :pin "1453e7486dd0921f0319f21dd8c8b603e4eb7300"))
