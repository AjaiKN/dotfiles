;;; term/mistty/config.el -*- lexical-binding: t; -*-
(defvar +mistty-widened-commands (list #'mouse-secondary-save-then-kill
                                       #'mouse-set-secondary
                                       #'mouse-drag-secondary
                                       #'mouse-kill-secondary
                                       #'mouse-yank-secondary
                                       #'mouse-start-secondary
                                       #'akn/copy-secondary-selection
                                       #'akn/insert-secondary-selection
                                       #'akn/set-region-to-secondary-selection
                                       #'akn/set-secondary-selection-to-region
                                       #'akn/swap-region-and-secondary-selection))

(use-package! mistty
  :defer t
  :config
  ;; C-d
  (map!
   :map mistty-prompt-map
   :nviemorg "C-d" #'+mistty/C-d

  ;; <up> and <down> for history
   "<up>"   #'mistty-send-C-p
   "<down>" #'mistty-send-C-n

  ;; <right> to accept autosuggestion
   "<right>" (akn/cmds! (+mistty--in-input-p) #'mistty-send-key)

   "DEL" (akn/cmds! (not (use-region-p)) #'mistty-backward-delete-char)

   :i "<escape>" (cmd! (mistty-send-key 1 (kbd "ESC")) (evil-normal-state))

   :nviemorg "<backtab>" #'+mistty/shift-tab-command
   :nviemorg "S-<tab>"   #'+mistty/shift-tab-command
   :nviemorg "S-TAB"     #'+mistty/shift-tab-command)

  (map! :map mistty-term-key-map
        "S-<tab>"   "\e[Z"
        "<backtab>" "\e[Z"
        "S-TAB"     "\e[Z")

  (map!
   :map mistty-mode-map
   ;; nice for canceling zsh auto-complete menu
   ;; (because I have `bindkey -M menuselect ^G send-break' set)
   :i "C-g" #'mistty-self-insert

   :i "<escape>" (cmd! (mistty-send-key 1 (kbd "ESC"))))

  (when (modulep! :editor evil)
    (add-to-list 'evil-insert-state-modes 'mistty-mode))

  ;; HACK: my prompt detection stuff doesn't work on the very first prompt, so
  ;; go immediately to the second.
  (add-hook! 'mistty-mode-hook
    (defun +mistty--ret-at-beginning-h ()
      (let ((mistty-buffer (current-buffer)))
        (run-with-timer
         0 nil
         (lambda ()
           (with-current-buffer mistty-buffer
             (mistty-send-key 1 (kbd "RET"))))))))

  ;; prompt fields
  (akn/advice-remove #'mistty--realize-possible-prompt :before #'+mistty--make-prompt-field-a)
  (akn/advise-letf! mistty--interact-next  (+mistty--ignore-read-only1-a) (inhibit-read-only t))
  (akn/advise-letf! mistty--queue-interact (+mistty--ignore-read-only2-a) (inhibit-read-only t))

  ;; disable corfu
  (add-hook 'mistty-mode-hook #'+mistty--turn-off-corfu-mode)

  ;; settings
  (setq! mistty-skip-empty-spaces t
         mistty-stable-delay-s 0.05
         mistty-timeout-s 0.1)

  ;; setting cursor right
  (advice-add #'mistty--post-command :after #'mistty--set-cursor-right-a)
  (advice-add #'mistty--pre-command :after #'+mistty-narrow-to-input-h)
  (akn/advice-remove #'mistty--pre-command :after #'+mistty--make-prompt-field-a)
  (akn/advice-remove #'mistty--post-command :before #'+mistty--remove-prompt-field-a)

  ;; strict mouse mode
  (add-hook 'mistty-mode-hook #'+mistty-strict-mouse-mode)

  ;; narrow to input advice
  (akn/advice-remove #'evil-delete-back-to-indentation         :around #'+mistty--narrow-to-input-a)
  (akn/advice-remove #'doom/backward-to-bol-or-indent          :around #'+mistty--narrow-to-input-a)
  (akn/advice-remove #'doom/forward-to-last-non-comment-or-eol :around #'+mistty--narrow-to-input-a)

  nil)
