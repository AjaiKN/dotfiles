;;; ui/mini-frame/config.el -*- lexical-binding: t; -*-

(use-package! mini-frame
  :defer t
  :defer-incrementally t
  :ghook 'doom-first-input-hook
  :config
  (setq! mini-frame-show-parameters
         '((left . 0.5)
           (top . 0.3)
           (width . 0.8)
           (height . 1))
         mini-frame-completions-show-parameters
         '((height . 0.25)
           (width . 1.0)
           (left . 0.5))
         mini-frame-internal-border-color
         nil)
  (mini-frame-mode))
