;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "a6501cbfe3db791f9ca17fd986c7202a87f3adb8")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))

(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "c2a46c0c9fa76103279f5f6ac97d7a6d9d6bcb7f"
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold"))
  (package! ts-fold :pin "3439756b5bbab83f65914d86b093d8c237eb7275"
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outli :pin "6527512470b450b4d1c0d8bba69888de19f0c124"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! outline-indent :pin "707c5b75af9934776504662d03b68b51de93b26c")

(package! comint-fold :pin "9b9f2bbc762c846bf328e698413391db149cc759"
  :recipe (:host github :repo "jdtsmith/comint-fold"))
