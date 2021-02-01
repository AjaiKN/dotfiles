;;; ui/prism/config.el -*- lexical-binding: t; -*-

(require 'akn)

(defvar-local +prism--local-set-face-cookies nil)

(use-package! prism
  :defer t
  ;; :defer-incrementally (cl-lib color face-remap thingatpt subr-x compat dash)
  :config
  ;; dim parens
  (setq! prism-parens t)

  (add-hook! 'prism-mode-hook
    (defun +prism-set-faces-h ()
      (if prism-mode
          (setq-local
           +prism--local-set-face-cookies
           (akn/local-set-faces!
             '(font-lock-builtin-face :slant oblique) ; in lisp, these are :keywords
             '(font-lock-keyword-face :weight bold :underline t) ; in lisp, these are macros
             '(font-lock-function-name-face :weight bold)
             (when (derived-mode-p 'lisp-data-mode)
               '(font-lock-constant-face :weight bold))))
        (akn/local-unset-faces! +prism--local-set-face-cookies)))))

;;; Themes

(after! prism
  (prism-set-colors :num 16
    :desaturations (cl-loop for i from 0 below 16
                            collect (* i 2.5))
    :lightens (cl-loop for i from 0 below 16
                       collect (* i 2.5))
    :colors (list "sandy brown" "dodgerblue" "medium sea green")

    :comments-fn
    (lambda (color)
      (prism-blend color
                   (face-attribute 'font-lock-comment-face :foreground) 0.25))

    :strings-fn
    (lambda (color)
      (prism-blend color "white" 0.5))))

(after! prism
  (defalias '+prism/shuffle #'prism-randomize-colors)

  ;; contains a bunch of themes
  (defun +prism/visit-notes ()
    (interactive)
    (find-file "~/.config/emacs/.local/straight/repos/prism.el/notes.org")))
