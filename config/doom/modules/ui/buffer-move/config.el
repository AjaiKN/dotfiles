;;; ui/buffer-move/config.el -*- lexical-binding: t; -*-

(use-package! buffer-move
  :defer t
  :defer-incrementally (windmove cl-lib)
  :config
  (setq! buffer-move-behavior 'move)
  (defadvice! akn/undedicate-before-move-a (orig-fn direction)
    :around #'buf-move-to
    (with-window-non-dedicated (selected-window)
      (with-window-non-dedicated (windmove-find-other-window direction)
        (apply orig-fn direction nil)))))
