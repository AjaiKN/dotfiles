;;; app/everywhere/config.el -*- lexical-binding: t; -*-

;;;; emacs-everywhere
(use-package! emacs-everywhere
  ;; Entry points into this package are autoloaded; i.e. the `emacs-everywhere'
  ;; function, meant to be called directly via emacsclient. See this module's
  ;; readme for details.
  :defer-incrementally t
  :defer t
  :config
  (set-yas-minor-mode! 'emacs-everywhere-mode)

  ;; HACK: Inhibit MAJOR-MODE-local-vars-hook in emacs-everywhere buffers,
  ;;   because Doom commonly starts servers and other extraneous services on
  ;;   this hook, which will rarely work well in emacs-everywhere's temporary
  ;;   buffers anyway.
  (setq-hook! 'emacs-everywhere-init-hooks doom-inhibit-local-var-hooks t)

  (after! doom-modeline
    (doom-modeline-def-segment emacs-everywhere
      (concat
       (doom-modeline-spc)
       (when (emacs-everywhere-markdown-p)
         (concat
          (nerd-icons-octicon "nf-oct-markdown" :face 'nerd-icons-green :v-adjust 0.02)
          (doom-modeline-spc)))
       (propertize (emacs-everywhere-app-class emacs-everywhere-current-app)
                   'face 'doom-modeline-project-dir)
       (doom-modeline-spc)
       (propertize (truncate-string-to-width
                    (emacs-everywhere-app-title emacs-everywhere-current-app)
                    45 nil nil "…")
                   'face 'doom-modeline-buffer-minor-mode)))
    (doom-modeline-def-modeline 'emacs-everywhere
      '(bar modals emacs-everywhere buffer-position
        word-count parrot selection-info)
      '(input-method major-mode check
        #("  " 0 1 ; "Exit to app" icon + a little padding
          (rear-nonsticky t
           display (raise -0.25)
           face (:inherit doom-modeline-emphasis :family "Material Icons" :height 1.1)
           help-echo "This is an Emacs Everywhere window"))))
    (add-hook! 'emacs-everywhere-mode-hook
      (defun +everywhere-set-modeline ()
        (doom-modeline-set-modeline 'emacs-everywhere))))
  (add-hook! 'emacs-everywhere-init-hooks
    (defun +everywhere--clear-persp-info-h ()
      (when (bound-and-true-p persp-mode)
        (setq persp-emacsclient-init-frame-behaviour-override nil)))
    #'+everywhere--frame-setup-h
    #'+everywhere--buffer-setup-h)

  (setq! emacs-everywhere-major-mode-function #'markdown-mode)
  ;; (setq! emacs-everywhere-clipboard-sleep-delay 0.01)
  ;; (add-hook! 'emacs-everywhere-init-hooks
  ;;            ;; one of the hooks is the one that copies, so we can't switch to this window until
  ;;            ;; that's done
  ;;            :depth -100
  ;;            (defun akn/focus-emacs ()
  ;;              ;; (async-shell-command "osascript -e 'tell application \"Emacs\" to activate'")))
  ;;              ;; (start-process-shell-command "emacs-everywhere-command-c" nil "osascript -e 'tell application \"System Events\" to keystroke \"c\" using {command down}'")))
  ;;              ;; (sleep-for emacs-everywhere-clipboard-sleep-delay)
  ;;              ;; (start-process-shell-command "applescript-focus-emacs"
  ;;              ;;                              nil
  ;;              ;;                              "osascript -e 'tell application \"Emacs\" to activate'")))
  ;;              (start-process "applescript-focus-emacs"
  ;;                             nil
  ;;                             "osascript")))
  ;; (sleep-for emacs-everywhere-clipboard-sleep-delay)))
  ;; (yank)))
  (when (executable-find "pbcopy")
    (setq! emacs-everywhere-copy-command (list "sh" "-c" "pbcopy < %f"))))

;;;; atomic-chrome (GhostText in Firefox)
;; https://discourse.doomemacs.org/t/emacs-for-editing-anything-anywhere-in-the-browser-discord-etc/129
(use-package! atomic-chrome
  :unless akn/terminal-only-p
  :defer-incrementally t
  :defer 5
  :after-call (akn/emacs-focus-out-hook server-mode-on-hook)
  :config
  (map! :map atomic-chrome-edit-mode-map
        [remap server-edit] #'atomic-chrome-close-current-buffer)

  (add-hook 'atomic-chrome-edit-mode-on-hook #'+everywhere--buffer-setup-h)
  (add-hook 'atomic-chrome-edit-done-hook #'+everywhere--focus-firefox-h)

  (defadvice! +everywhere--setup-frame-a (ret)
    :filter-return #'atomic-chrome-show-edit-buffer
    (with-demoted-errors "+everywhere--setup-frame-a: %S"
      (when (and (eq atomic-chrome-buffer-open-style 'frame) (framep ret))
        (with-selected-frame ret
          (+everywhere--frame-setup-h))))
    ret)
  (defadvice! +everywhere--mark-unmodified-a (&rest _)
    :after #'atomic-chrome-create-buffer
    :after #'atomic-chrome-update-buffer
    (when (and atomic-chrome-edit-mode (not buffer-file-name))
      (set-buffer-modified-p nil)))

  (setq atomic-chrome-buffer-open-style 'frame
        atomic-chrome-buffer-frame-width  80
        atomic-chrome-buffer-frame-height 12)
  (atomic-chrome-start-server))
