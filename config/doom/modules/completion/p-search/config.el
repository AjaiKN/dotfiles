;;; completion/p-search/config.el -*- lexical-binding: t; -*-

(use-package! p-search
  :defer t
  :commands (p-search)
  :config
  (+evil-collection-init 'p-search)
  (add-hook 'p-search-mode-hook #'doom-mark-buffer-as-real-h)
  (p-search-peruse-mode))
