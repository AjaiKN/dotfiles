;;; ui/highlight-symbol/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! symbol-overlay :pin "85d100b0cca35b70cee1b260e09af8e1fb2fcc08")
(when (modulep! :editor multiple-cursors)
  (package! symbol-overlay-mc :pin "f8b3de78ab44b12a71e1e6d1689c346f00044065"))
