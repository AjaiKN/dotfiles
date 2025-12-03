;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "f71f374d28a83e5f15612fa64aac1b2e78be2dcd")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))

(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "8182ae243a57c3f418ea64ac1e594a210f5dcf16"
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold"))
  (package! ts-fold :pin "9ccab656670b66bba853a1e41d2e27d9c497ee0c"
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outli :pin "009e74c1757143040a0427f477ae882107b14592"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! outline-indent :pin "832595bc0f6699171e9ebcafce3952e2a151cb26")

(package! comint-fold :pin "0dd06663d4e666c20650c3556fe4985731dd600f"
  :recipe (:host github :repo "jdtsmith/comint-fold"))
