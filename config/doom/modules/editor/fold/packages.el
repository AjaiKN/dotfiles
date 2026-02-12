;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "f71f374d28a83e5f15612fa64aac1b2e78be2dcd")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))

(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "c5f0a95550d16dc21871347c0ddd97d9328305dd"
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold"))
  (package! ts-fold :pin "aaef85cd01f9a7fc20f27af8f0ba614aa1b3c7f5"
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outli :pin "009e74c1757143040a0427f477ae882107b14592"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! outline-indent :pin "429c8a67a962d99ceb0207ce62c34b833aee0896")

(package! comint-fold :pin "0dd06663d4e666c20650c3556fe4985731dd600f"
  :recipe (:host github :repo "jdtsmith/comint-fold"))
