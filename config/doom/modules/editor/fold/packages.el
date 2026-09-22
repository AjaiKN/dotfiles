;;; editor/fold/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; TODO: try https://github.com/jamescherti/kirigami.el / https://www.jamescherti.com/emacs-the-definitive-guide-to-code-folding/

(package! hideshow :built-in t)

(package! vimish-fold :pin "f71f374d28a83e5f15612fa64aac1b2e78be2dcd")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))

(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "cc1003b730a3167b972cc8400dffe19be7988fc7"
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold"))
  (package! ts-fold :pin "1200261a1d8e47adcbf36495220b2795893eb90a"
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outli :pin "36a5048805f363b3161c8d0a96cd904351231c91"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! outline-indent :pin "41b5065b5c9a2a0b8a796cab27e60aad00d762f0")

(package! comint-fold :pin "0dd06663d4e666c20650c3556fe4985731dd600f"
  :recipe (:host github :repo "jdtsmith/comint-fold"))
