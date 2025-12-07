;;; tools/mason/config.el -*- lexical-binding: t; -*-

(use-package! mason
  :hook (doom-first-input . mason-ensure))
