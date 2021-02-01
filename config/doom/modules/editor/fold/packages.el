;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold :pin "a6501cbfe3db791f9ca17fd986c7202a87f3adb8")
(when (modulep! :editor evil)
  (package! evil-vimish-fold :pin "b6e0e6b91b8cd047e80debef1a536d9d49eef31a"))

(when (modulep! :tools tree-sitter)
  (package! treesit-fold :pin "6628b7cce585328e05d810b5505e4fdb9306f24f"
    :recipe (:host github :repo "emacs-tree-sitter/treesit-fold"))
  (package! ts-fold :pin "6eed69cf2b97dce9c6ad329710ed42f0ad674f76"
    :recipe (:host github :repo "emacs-tree-sitter/ts-fold")))

(package! outli :pin "c716ed5b4b14ac3694d75529cb05b16ef6a9447a"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! outline-indent :pin "56595594ea59fc10c6a8f7293883bede14ecb53e")

(package! comint-fold :pin "9b9f2bbc762c846bf328e698413391db149cc759"
  :recipe (:host github :repo "jdtsmith/comint-fold"))
