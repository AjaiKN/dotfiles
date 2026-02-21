;;; editor/tempel/config.el -*- lexical-binding: t; -*-

(use-package! tempel
  :defer t
  :config
  (setq! tempel-path (list (expand-file-name "snippets.eld" doom-user-dir))))
