;;; ui/modeline-scrollbar/config.el -*- lexical-binding: t; -*-

(use-package! mlscroll
  :unless akn/terminal-only-p
  :defer t
  :ghook 'doom-modeline-mode-hook '+modeline-global-mode-hook
  :config
  ;; HACK: seemed to be causing errors in terminal
  (defadvice! +mlscroll--not-on-terminal-a (&rest _)
    :before-while #'mlscroll--update-size
    :before-while #'mlscroll--calculate-faces
    :before-while #'mlscroll-layout
    :before-while #'mlscroll-mode-line
    (display-graphic-p)))
