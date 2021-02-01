;;; tools/age/config.el -*- lexical-binding: t; -*-

(use-package! age
  :defer t
  :init
  (setenv "PINENTRY_PROGRAM" "pinentry-mac")
  ;; (when (boundp 'akn/age-default-identity) (customize-set-variable 'age-default-identity akn/age-default-identity))
  ;; (when (boundp 'akn/age-default-recipient) (customize-set-variable 'age-default-identity akn/age-default-recipient))
  (defvar akn/age-regex (rx ".age" eos))
  ;; (setq! file-name-handler-alist (cons `(,akn/age-regex . age-file-handler) file-name-handler-alist))
  (add-to-list 'file-name-handler-alist `(,akn/age-regex . age-file-handler))
  ;; (setq! auto-mode-alist (cons (list akn/age-regex nil 'age-file-enable) auto-mode-alist))
  (add-to-list 'auto-mode-alist `(,akn/age-regex nil age-file-enable))

  (after! undo-fu-session
    (add-to-list 'undo-fu-session-incompatible-files akn/age-regex))

  ;; :custom
  ;; (age-program "rage")
  ;; (age-always-use-default-keys nil)
  ;; (age-default-identity nil)
  ;; (age-default-recipient nil)
  ;; (age-default-identity (and (boundp 'akn/age-default-identity) akn/age-default-identity))
  ;; (age-default-recipient (and (boundp 'akn/age-default-recipient) akn/age-default-recipient))

  :config
  (when (executable-find "rage")
   (setq! age-program "rage"))
  (shut-up (age-file-enable)))
