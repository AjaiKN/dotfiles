;; -*- no-byte-compile: t; -*-
;;; ui/highlight-symbol/packages.el

(package! symbol-overlay :pin "6151f4279bd94b5960149596b202cdcb45cacec2")
(when (modulep! :editor multiple-cursors)
  (package! symbol-overlay-mc :pin "f8b3de78ab44b12a71e1e6d1689c346f00044065"))
