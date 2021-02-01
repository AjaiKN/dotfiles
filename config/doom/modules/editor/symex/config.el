;;; editor/symex/config.el -*- lexical-binding: t; -*-

(use-package! symex
  :defer t
  :bind ("s-'" . symex-mode-interface)

  :custom
  (symex-modal-backend 'evil)

  :init
  (setq evil-symex-state-cursor '(bar . 0))
  ;; (defadvice! +symex-color-a ()
  ;;  :after #'+evil-update-cursor-color-h
  ;;  (put 'cursor 'evil-symex-color (face-foreground 'ffap)))
  ;; (defun +evil-symex-cursor-fn ()
  ;;   (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

  :config
  ;; https://github.com/drym-org/symex.el?tab=readme-ov-file#up-and-down
  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)
          ("s-'" . symex-escape-higher)))
  (add-hook! 'symex-editing-mode-hook
    (defun +symex--disable-show-paren-h ()
      (akn/mode-set symex-editing-mode
        show-paren-mode nil)
      (show-paren-local-mode (if show-paren-mode 1 -1))
      (akn/mode-add-hook! symex-editing-mode 'post-command-hook :local
        (defun akn/symex--update-overlay-h ()
          (unless (memq this-command akn/all-scroll-commands) ;TODO: in private config
            (symex--update-overlay))))))
  (symex-initialize))

(add-hook! 'symex-mode-on-hook
  (defun +symex--parinfer-off-h ()
    (when (bound-and-true-p parinfer-rust-mode)
      (parinfer-rust-mode -1))))
(after! parinfer-rust-mode
  (add-to-list 'parinfer-rust-troublesome-modes 'symex-mode))
(add-hook! 'parinfer-rust-mode-on-hook
  (defun +symex--parinfer-off-h ()
    (when (bound-and-true-p symex-mode)
      (symex-mode -1))))

(use-package! rigpa
  :defer t
  :config
  (rigpa-initialize)
  (setq rigpa-mode t)
  (blink-cursor-mode)

  ;; temporary workaround for https://github.com/countvajhula/rigpa/issues/9
  (remove-hook 'evil-symex-state-exit-hook #'symex-disable-editing-minor-mode)

  ;; custom config
  (setq rigpa-show-menus nil)

  ;; navigating meta modes
  (global-unset-key (kbd "H-m"))
  (map!
   :desc "last tower"        "H-m H-m"    #'rigpa-flashback-to-last-tower
   :desc "mode mode"         "M-<escape>" #'rigpa-enter-mode-mode
   :desc "mode mode"         "H-<escape>" #'rigpa-enter-mode-mode
   :desc "tower mode"        "C-<escape>" (cmd! (when (eq rigpa--complex rigpa-meta-complex) (rigpa-exit-mode-mode))
                                                (rigpa-enter-tower-mode))
   :desc "ground"            "M-<return>" (cmd! (when (eq rigpa--complex rigpa-meta-complex) (rigpa-enter-selected-level) (let ((ground (rigpa--get-ground-buffer)))
                                                                                                                            (rigpa-exit-mode-mode)
                                                                                                                            (switch-to-buffer ground))))
   :desc "ground"            "H-<return>" (cmd! (when (eq rigpa--complex rigpa-meta-complex) (rigpa-enter-selected-level) (let ((ground (rigpa--get-ground-buffer)))
                                                                                                                            (rigpa-exit-mode-mode)
                                                                                                                            (switch-to-buffer ground))))
   :desc "tower->mode mode"  "C-<return>" (cmd! (when (eq rigpa--complex rigpa-meta-tower-complex) (rigpa-exit-tower-mode) (rigpa-enter-mode-mode)))
   :desc "normal mode"       "H-o" #'evil-normal-state
   :desc "symex mode"        "H-y" (cmd! (rigpa-enter-mode "symex"))
   :desc "symex mode"        "H-;" (cmd! (rigpa-enter-mode "symex"))
   :desc "window mode"       "H-w" (cmd! (rigpa-enter-mode "window"))
   :desc "view mode"         "H-v" (cmd! (rigpa-enter-mode "view"))
   :desc "char mode"         "H-x" (cmd! (rigpa-enter-mode "char"))
   :desc "activity mode"     "H-a" (cmd! (rigpa-enter-mode "activity"))
   :desc "text mode"         "H-z" (cmd! (rigpa-enter-mode "text"))
   :desc "history mode"      "H-g" (cmd! (rigpa-enter-mode "history"))
   :desc "system mode"       "H-i" (cmd! (rigpa-enter-mode "system"))
   :desc "buffer mode"       "H-b" (cmd! (rigpa-enter-mode "buffer"))
   :desc "file mode"         "H-f" (cmd! (rigpa-enter-mode "file"))
   :desc "tab mode"          "H-t" (cmd! (rigpa-enter-mode "tab"))
   :desc "line mode"         "H-l" (cmd! (rigpa-enter-mode "line"))
   :desc "app mode"          "H-e" (cmd! (rigpa-enter-mode "application"))
   :desc "word mode"         "H-r" (cmd! (rigpa-enter-mode "word"))))
