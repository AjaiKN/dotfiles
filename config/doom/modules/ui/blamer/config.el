;;; ui/blamer/config.el -*- lexical-binding: t; -*-

(use-package! blamer
  :bind (("s-i" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   ;; :height 140
                   :italic t))))
