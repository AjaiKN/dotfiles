;;; emacs/secondary-selection/config.el -*- lexical-binding: t; -*-

(after! mouse
  (map!
   ;; "s-c" (akn/cmds! (secondary-selection-exist-p) #'akn/copy-secondary-selection)
   "M-S-<mouse-1>" #'mouse-yank-secondary)

  (setq mouse-yank-at-point nil)

  (add-hook! 'doom-escape-hook :depth 95
             #'akn/remove-secondary-selection)

  (defadvice! akn/call-before-secondary-selection-hook-a (&rest _)
    ;; :before #'mouse-start-secondary
    :before #'mouse-drag-secondary
    ;; :before #'mouse-set-secondary
    (doom-run-hooks 'akn/before-secondary-selection-hook))
  (defadvice! akn/call-after-secondary-selection-hook-a (&rest _)
    :after #'mouse-drag-secondary
    :after #'mouse-secondary-save-then-kill
    (run-hooks 'akn/after-secondary-selection-hook))

  (add-hook 'akn/after-secondary-selection-hook #'akn/copy-secondary-selection))
