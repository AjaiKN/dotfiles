;; -*- no-byte-compile: t; -*-
;;; emacs/casual/packages.el

(package! casual :pin "5e004a06eef3b66865b785bca51512cdb89ff6a9")
(package! casual-avy :pin "6716c12e9b7ba8325dab05d55c1ea5cc2b9a44f1")
(when (modulep! :ui highlight-symbol)
  (package! casual-symbol-overlay :pin "1453e7486dd0921f0319f21dd8c8b603e4eb7300"))
