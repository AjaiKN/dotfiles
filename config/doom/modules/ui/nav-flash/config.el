;;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(defvar +nav-flash-exclude-commands
  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point
    org-find-file org-find-file-at-mouse)
  "A list of commands that should not trigger nav-flash.")

(defvar +nav-flash-exclude-modes
  '(so-long-mode special-mode comint-mode term-mode vterm-mode eat-mode)
  "List of major modes where nav-flash won't automatically trigger.")


;;
;;; Packages

(use-package! pulsar
  ;; doesn't seem to be working well in the terminal
  :unless (bound-and-true-p akn/terminal-only-p)
  :defer t
  :defer-incrementally t
  :commands pulsar-global-mode
  :init
  ;; NOTE In :tools lookup `recenter' is hooked to a bunch of jumping
  ;; commands, which will trigger nav-flash.
  (add-hook! '(imenu-after-jump-hook
               better-jumper-post-jump-hook
               counsel-grep-post-action-hook
               consult-after-jump-hook
               dumb-jump-after-jump-hook)
             #'+nav-flash-blink-cursor-maybe-h)

  (add-hook 'doom-switch-window-hook #'+nav-flash-blink-cursor-maybe-h)

  ;; `org'
  (add-hook 'org-follow-link-hook #'+nav-flash-delayed-blink-cursor-h)

  ;; `persp-mode'
  (after! persp-mode
    (add-hook 'persp-activated-functions #'+nav-flash-delayed-blink-cursor-h))

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'+nav-flash-blink-cursor-a)

  ;; `evil'
  (advice-add #'evil-window-top    :after #'+nav-flash-blink-cursor-a)
  (advice-add #'evil-window-middle :after #'+nav-flash-blink-cursor-a)
  (advice-add #'evil-window-bottom :after #'+nav-flash-blink-cursor-a)
  (advice-add #'recenter           :after #'+nav-flash-blink-cursor-a)

  ;; Bound to `ga' for evil users
  (advice-add #'what-cursor-position :after #'+nav-flash-blink-cursor-a)

  (advice-add #'vlf-move-to-chunk :after #'+nav-flash-delayed-blink-cursor-a)

  ;; https://github.com/torgeir/.config/emacs
  :config
  (setq pulsar-delay 0.055
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow
        ;; Seems to have a big performance impact when opening new buffers, even
        ;; if the buffer was created programmatically For example, when
        ;; parinfer-rust opens a new diff-mode buffer after every command,
        ;; resolving aliases introduces a bunch of input lag.
        pulsar-resolve-pulse-function-aliases nil)
  (pushnew! pulsar-pulse-functions
            #'beginning-of-buffer
            #'end-of-buffer
            #'akn/dired-goto-beginning
            #'akn/dired-goto-end
            #'+fold/open-all
            #'+fold/close-all
            #'+fold/table-of-contents
            #'+fold/overview
            #'+fold/all-headings
            #'+fold/unfold-all-headings
            #'+fold/outline-cycle-all
            #'+fold/outline-cycle-all-simple)
  (pulsar-global-mode)

  (after! so-long
    (add-to-list 'so-long-minor-modes 'pulsar-mode)))
