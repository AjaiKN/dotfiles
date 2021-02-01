;;; lang/kdl/config.el -*- lexical-binding: t; -*-

(use-package! kdl-ts-mode
  :mode "\\.kdl\\'"
  :init
  (set-tree-sitter! 'kdl-mode 'kdl-ts-mode
    '((kdl :url "https://github.com/tree-sitter-grammars/tree-sitter-kdl"))))
