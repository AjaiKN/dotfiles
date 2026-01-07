;;; tools/mise/config.el -*- lexical-binding: t; -*-

(use-package! mise
  :ghook ('doom-first-file-hook #'global-mise-mode))
