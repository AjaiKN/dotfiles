;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "a6501cbfe3db791f9ca17fd986c7202a87f3adb8")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))

(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "7502b7ab6964cccc49c140d61baaa3bf3c73f769"
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold"))
  (package! ts-fold :pin "5585568bf689f19e9e1454b5d78395bb39106855"
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outli :pin "6527512470b450b4d1c0d8bba69888de19f0c124"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! outline-indent :pin "f90eeb3aa4a0ca7d9c369712e20d219d7f156411")

(package! comint-fold :pin "9b9f2bbc762c846bf328e698413391db149cc759"
  :recipe (:host github :repo "jdtsmith/comint-fold"))
