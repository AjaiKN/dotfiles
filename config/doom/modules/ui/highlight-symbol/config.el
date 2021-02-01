;;; ui/highlight-symbol/config.el -*- lexical-binding: t; -*-

(require 'akn)

(use-package! symbol-overlay
  :defer t
  :defer-incrementally t
  :autoload akn/symbol-overlay-global-mode
  :init
  ;; TODO: improve face, especially on comments, and when region active.
  ;; Use `symbol-overlay-priority', `symbol-overlay-overlay-created-functions', `overlay-put', `delete-overlay', `overlay-start', `overlay-end'

  (add-hook! 'doom-first-buffer-hook
    (akn/after-idle! (0.5)
      (require 'symbol-overlay)))

  :config
  (define-globalized-minor-mode akn/symbol-overlay-global-mode
    symbol-overlay-mode symbol-overlay-mode
    :predicate '(prog-mode)
    :group 'akn)
  (akn/symbol-overlay-global-mode)

  (defadvice! akn/symbol-overlay-evil-normal-state-a (&optional noerror)
    :override #'symbol-overlay-get-symbol
    (or (and (not (region-active-p))
             (not (secondary-selection-exist-p))
             (let ((sym (thing-at-point 'symbol)))
               (if (memq (bound-and-true-p evil-state) '(normal motion))
                   (and (equal sym (save-excursion (ignore-errors (right-char))
                                                   (thing-at-point 'symbol)))
                        sym)
                 sym)))
        (unless noerror (user-error "No symbol at point"))))
  (add-hook! 'symbol-overlay-overlay-created-functions
    (defun akn/symbol-overlay-h (ov)
      (when (or (nth 3 (doom-syntax-ppss (overlay-start ov)))  ;in a string
                (nth 4 (doom-syntax-ppss (overlay-start ov)))) ;in a comment
        (delete-overlay ov))))
  (add-hook! '(activate-mark-hook akn/before-secondary-selection-hook)
             #'symbol-overlay-remove-temp)

  (after! so-long (add-to-list 'so-long-minor-modes 'symbol-overlay-mode))

  (setq symbol-overlay-temp-highlight-single t)

  (map! :desc "Next other highlighted symbol"
        "M-n" (akn/cmds! symbol-overlay-keywords-alist #'symbol-overlay-switch-forward)
        :desc "Previous other highlighted symbol"
        "M-p" (akn/cmds! symbol-overlay-keywords-alist #'symbol-overlay-switch-backward)
        :desc "Remove all highlights"
        "<f8>" #'symbol-overlay-remove-all)
  (setq symbol-overlay-map (make-sparse-keymap))
  (map! :map symbol-overlay-map
        "r"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-rename)
        ;; TODO: add symbol-overlay-mc binding
        "M-i" #'symbol-overlay-put
        ;; "C-h" #'symbol-overlay-map-help
        "N"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-jump-prev)
        "p"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-jump-prev)
        "n"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-jump-next)
        "<"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-jump-first)
        ">"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-jump-last)
        "y"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-save-symbol)
        "f"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-toggle-in-scope) ;in current defun only
        ;; "e"   #'symbol-overlay-echo-mark
        "d"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-jump-to-definition)
        "s"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-isearch-literally)
        "S"   (akn/cmds! (memq evil-state '(motion normal emacs visual)) #'symbol-overlay-query-replace)
        "s-?" #'casual-symbol-overlay-tmenu
        "C-o" #'casual-symbol-overlay-tmenu)

  (add-hook! 'doom-escape-hook :depth 88
    (defun akn/remove-this-symbol-overlay-h ()
      (when (and (symbol-overlay-get-symbol t)
                 (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
        (symbol-overlay-put)
        t)))
  (add-hook! 'doom-escape-hook :depth 92
    (defun akn/remove-all-symbol-overlays-h ()
      (when symbol-overlay-keywords-alist
        (call-interactively #'symbol-overlay-remove-all)
        t)))

  (advice-add #'symbol-overlay-idle-timer :around #'akn/ignore-errors-a))

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
