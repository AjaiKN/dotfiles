;;; completion/autosuggest/config.el -*- lexical-binding: t; -*-
;; AKA autosuggest overlay

;; also see capf-autosuggest

;; https://eshelyaron.com/posts/2023-11-17-completion-preview-in-emacs.html
(use-package! completion-preview
  :defer-incrementally t
  :ghook ('doom-first-buffer-hook #'global-completion-preview-mode)
  :config
  (global-completion-preview-mode)
  (add-to-list 'global-completion-preview-modes 'minibuffer-mode)
  (pushnew! (alist-get 'not global-completion-preview-modes) 'org-mode)

  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2)

  (setq completion-preview-idle-delay 0.01)
  ;; (setq completion-preview-idle-delay nil)
  (define-advice completion-preview--show (:around (fn &rest args) +preview--a)
    "To avoid excessive jumping back and forth, only use a delay if at the
end of the line."
    (unless (and (minibufferp) (fboundp 'vertico--command-p) (vertico--command-p nil (current-buffer)))
      (let ((completion-preview-idle-delay (if (eolp) completion-preview-idle-delay nil)))
        (apply fn args))))

  (define-advice completion-preview--update (:around (fn &rest args) +preview--a)
    (let ((completion-at-point-functions (cons #'+preview-history-capf completion-at-point-functions)))
      (apply fn args)))
  (setq-hook! '(eshell-mode-hook comint-mode-hook minibuffer-mode-hook)
    completion-preview-minimum-symbol-length nil)

  (after! corfu
    ;; also `corfu-sort-override-function'?
    (setq completion-preview-sort-function
          (lambda (&rest args) (apply corfu-sort-function args))))

  ;; Non-standard commands to that should show the preview:

  (pushnew! completion-preview-commands
            #'org-self-insert-command
            #'paredit-backward-delete
            #'quail-self-insert-command
            #'eshell-self-insert-command
            #'semantic-complete-self-insert)

  ;; Bindings that take effect when the preview is shown:
  ;; Cycle the completion candidate that the preview shows
  (map! :map completion-preview-active-mode-map
        "<right>" (akn/cmds! (or (eolp) (derived-mode-p 'eshell-mode 'comint-mode 'minibuffer-mode))
                             #'completion-preview-insert)
        "M-n" #'completion-preview-next-candidate
        "M-p" #'completion-preview-prev-candidate
        ;; Convenient alternative to C-i after typing one of the above
        "M-i" #'completion-preview-insert))
