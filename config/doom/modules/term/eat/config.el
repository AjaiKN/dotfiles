;;; term/eat/config.el -*- lexical-binding: t; -*-
;;;;;; eat
;; https://old.reddit.com/r/emacs/comments/17nl7cw/shout_out_to_the_eat_terminal_emulator_package/k7u1ueu/
(use-package! eat
  :defer t
  :config
  (setq! process-adaptive-read-buffering nil
         read-process-output-max (* 4 1024 1024))

  ;; https://codeberg.org/akib/emacs-eat/issues/110
  (setq! eat-very-visible-cursor-type '(t nil nil) ; this is the most important one
         eat-shell-prompt-annotation-delay 0
         eat-default-cursor-type '(t nil nil))
  (setq! eat-kill-buffer-on-exit t
         eat-term-name #'+eat-term-name-fn)

  (set-ligatures! 'eat-mode nil)

  (when (modulep! :editor evil)
    (add-to-list 'evil-insert-state-modes 'eat-mode))

  (setq! eat-shell-prompt-annotation-position 'left-margin
         eat-shell-prompt-annotation-failure-margin-indicator "✖"
         eat-shell-prompt-annotation-running-margin-indicator "…"
         eat-shell-prompt-annotation-success-margin-indicator " "
         eat-enable-shell-prompt-annotation nil

         eat-enable-auto-line-mode t

         eat-line-input-send-function #'+eat-line-input-send-fn)

  (map! (:map eat-mode-map
         :i "<escape>" (akn/cmds! (not eat--line-mode) #'eat-self-input)
         :n "RET" (akn/cmds! eat--line-mode #'eat-line-send-input))
        (:map eat-line-mode-map
         "<up>"   #'eat-line-previous-matching-input-from-input
         "<down>" (akn/cmds! (null eat--line-input-ring-index) #'ignore
                             #'eat-line-next-matching-input-from-input))))
