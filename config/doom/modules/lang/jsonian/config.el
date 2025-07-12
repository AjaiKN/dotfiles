;;; lang/jsonian/config.el -*- lexical-binding: t; -*-

(use-package! jsonian
  :mode ("\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'" . jsonian-mode)
  :config
  (after! so-long
    (jsonian-no-so-long-mode))
  (after! flycheck
    (jsonian-enable-flycheck))

  (set-electric! 'jsonian-mode :chars '(?\n ?: ?{ ?}))

  (map! :map jsonian-mode-map
        [remap consult-imenu] #'jsonian-find
        [remap imenu]         #'jsonian-find
        :localleader
        :desc "Toggle comments (jsonc)" "c" (cmds! (derived-mode-p 'jsonian-c-mode) #'jsonian-mode #'jsonian-c-mode)
        :desc "Format region"           "f" #'jsonian-format-region
        :desc "Copy path"               "p" #'jsonian-path
        :desc "Edit string"             "s" #'jsonian-edit-string
        :desc "Up (enclosing item)"     "u" #'jsonian-enclosing-item)

  (defadvice! +jsonian-lines--narrow-to-line-a (fn &rest args)
    :around #'jsonian-path
    ;; :around #'jsonian-find
    ;; :around #'jsonian--cached-find-children
    ;; :around #'jsonian--display-path
    ;; :around #'jsonian--find-children
    ;; :around #'jsonian--find-completion
    ;; :around #'jsonian--path
    ;; :around #'jsonian--reconstruct-path
    ;; :around #'jsonian--snap-to-node
    ;; :around #'jsonian--valid-path
    ;; :around #'jsonian--completing-t
    (if (derived-mode-p '+jsonian-lines-mode)
        (save-restriction
          (narrow-to-region (line-beginning-position) (line-end-position))
          (apply fn args))
      (apply fn args))))

(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . +jsonian-lines-mode))
