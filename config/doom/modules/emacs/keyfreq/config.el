;;; emacs/keyfreq/config.el -*- lexical-binding: t; -*-

(use-package! keyfreq
  :defer-incrementally t
  :ghook 'doom-first-input-hook
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
