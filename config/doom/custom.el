(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" default))
 '(electric-operator-R-named-argument-style 'spaced)
 '(lsp-rust-analyzer-binding-mode-hints t)
 '(lsp-rust-analyzer-closure-capture-hints t)
 '(lsp-rust-analyzer-closure-return-type-hints "always")
 '(lsp-rust-analyzer-discriminants-hints "always")
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-closure-return-type-hints t)
 '(lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(lsp-rust-analyzer-display-reborrow-hints "always")
 '(lsp-rust-analyzer-expression-adjustment-hints "always")
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-directories
   '("/Users/ajainelson/prog/cs342-hw/" "/Users/ajainelson/.config/doom/"
     "~/.config/emacs/"))
 '(safe-local-variable-values
   '((eval conf-quote-normal 1) (checkdoc-allow-quoting-nil-and-t . t)
     (+tree-sitter-hl-enabled-modes) (Package . April)
     (Syntax . ANSI-Common-Lisp) (nameless-current-name . "electric-operator")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (jinx-local-words . "confpkg confpkgs smartparens tempbuffer")
     (package-lint--sane-prefixes . "\\`akn[/-]")
     (flycheck-disabled-checkers quote (emacs-lisp-checkdoc))
     (byte-compile-warnings quote (not make-local))
     (byte-compile-warnings quote (not make-local docstrings))
     (parinfer-rust-buffer-replace-strategy . safe)
     (parinfer-rust-buffer-replace-strategy . fast) (tab-always-indent . t)
     (swift-basic-offset . 2) (whitespace-style face lines indentation:space)
     (eval add-hook 'prog-mode-hook (lambda nil (whitespace-mode 1))
      (not :APPEND) :BUFFER-LOCAL)
     (eval let*
      ((x (dir-locals-find-file default-directory))
       (this-directory (if (listp x) (car x) (file-name-directory x))))
      (unless
          (or (featurep 'swift-project-settings)
              (and (fboundp 'tramp-tramp-file-p)
                   (tramp-tramp-file-p this-directory)))
        (add-to-list 'load-path (concat this-directory "utils") :append)
        (defvar swift-project-directory)
        (let ((swift-project-directory this-directory))
          (require 'swift-project-settings)))
      (set (make-local-variable 'swift-project-directory) this-directory))
     (eval font-lock-add-keywords nil
      `
      ((,(concat "("
                 (regexp-opt
                  '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op"
                    "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                  t)
                 "\\_>")
        1 'font-lock-variable-name-face)))
     (elisp-lint-ignored-validators "fill-column" "checkdoc")
     (akn/no-word-wrap . t) (comment-fill-column . 80)
     (elisp-lint-indent-specs (describe . 1) (it . 1))
     (elisp-lint-indent-specs (defhydra . 2))
     (byte-compile-warnings quote
      (not free-vars noruntime unresolved make-local))
     (elisp-lint-indent-specs (git-gutter:awhen . 1))
     (checkdoc-package-keywords-flag)
     (lisp-indent-function . common-lisp-indent-function))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+org-todo-project ((t (:inherit (bold font-lock-keyword-face org-todo)))))
 '(dirvish-collapse-dir-face ((t (:inherit dired-directory :foreground "orchid4"))))
 '(dirvish-collapse-file-face ((t (:inherit default :foreground "gray47"))))
 '(evil-mc-cursor-bar-face ((t (:height 1 :background "magenta" :foreground "magenta" :family "Linux Libertine"))))
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
