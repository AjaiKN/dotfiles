;;; lang/hare/config.el -*- lexical-binding: t; -*-

(use-package! hare-ts-mode
  :mode "\\.ha\\'"
  :init
  (set-tree-sitter! 'hare-mode 'hare-ts-mode
    '((hare :url "https://git.sr.ht/~ecs/tree-sitter-hare"))))
