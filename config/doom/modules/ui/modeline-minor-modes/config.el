;;; ui/modeline-minor-modes/config.el -*- lexical-binding: t; -*-

(require 'akn)

(setq doom-modeline-minor-modes t)
(add-hook 'doom-modeline-mode-hook #'minions-mode)

(defvar +modeline-minor-modes--promoted-keymaps nil)
(defun +modeline-minor-modes-promote-from-keymap (keymap)
  (unless (memq keymap +modeline-minor-modes--promoted-keymaps)
    (push keymap +modeline-minor-modes--promoted-keymaps)
    (map-keymap
     (lambda (_key def)
       (cond
        ((and (symbolp def)
              (commandp def)
              (string-match-p (rx "-mode" eos) (symbol-name def)))
         (push def minions-promoted-modes))
        ((keymapp def)
         (+modeline-minor-modes-promote-from-keymap def))))
     keymap)))


(after! minions
  ;; (+modeline-minor-modes-promote-from-keymap doom-leader-toggle-map)
  (+modeline-minor-modes-promote-from-keymap doom-leader-map)
  (+modeline-minor-modes-promote-from-keymap ctl-x-map)
  (pushnew! minions-prominent-modes
            #'superword-mode #'subword-mode)
  (pushnew! minions-promoted-modes
            ;; global
            #'diff-hl-flydiff-mode #'corfu-terminal-mode #'corfu-history-mode #'eros-mode #'corfu-popupinfo-mode #'global-anzu-mode #'which-key-mode #'global-fish-completion-mode #'global-ts-fold-mode #'treemacs-filewatch-mode #'treemacs-git-mode #'treemacs-fringe-indicator-mode #'speedrect-mode #'global-evil-surround-mode #'global-obsidian-mode #'magit-wip-mode #'yas-global-mode #'comint-fold-mode #'tab-bar-mode #'activities-tabs-mode #'activities-mode #'pulsar-global-mode #'solaire-global-mode #'projectile-mode #'menu-bar-mode #'akn/symbol-overlay-global-mode #'recentf-mode #'save-place-mode #'global-so-long-mode #'pixel-scroll-precision-mode #'ultra-scroll-mode #'global-diff-hl-mode #'envrc-global-mode #'global-fasd-mode #'global-git-commit-mode #'gcmh-mode #'global-hl-line-mode #'winner-mode #'show-paren-mode #'smartparens-global-mode #'ws-butler-global-mode #'global-completion-preview-mode #'global-tab-line-mode #'akn/electric-operator-global-mode #'undo-fu-session-global-mode #'undo-fu-mode #'global-flycheck-mode #'+global-word-wrap-mode #'savehist-mode #'better-jumper-mode #'global-corfu-mode #'vertico-multiform-mode #'vertico-mouse-mode #'vertico-mode #'nerd-icons-completion-mode #'marginalia-mode #'evil-goggles-mode #'evil-escape-mode #'evil-snipe-mode #'global-subword-mode #'keyfreq-mode #'keyfreq-autosave-mode #'repeat-help-mode #'repeat-mode #'kill-ring-deindent-mode #'dirvish-peek-mode #'+popup-mode #'global-ligature-mode #'window-divider-mode #'server-mode #'size-indication-mode #'column-number-mode #'minions-mode #'mlscroll-mode #'display-battery-mode #'display-time-mode #'doom-modeline-mode #'tabspaces-mode #'bufferlo-mode #'bufferlo-anywhere-mode #'delete-selection-mode #'tooltip-mode #'context-menu-mode #'global-goto-address-mode #'evil-mode #'akn/active-region-arrow-boundary-global-mode #'ns-auto-titlebar-mode #'tab-bar-history-mode #'el-patch-use-package-mode #'global-eldoc-mode #'electric-indent-mode #'file-name-shadow-mode #'global-font-lock-mode #'minibuffer-regexp-mode #'auto-encryption-mode #'transient-mark-mode #'akn/global-indent-guides-mode #'minimap-mode #'global-treesit-fold-mode #'global-treesit-fold-indicators-mode #'global-blamer-mode #'smartparens-global-mode #'smartparens-global-strict-mode #'show-smartparens-global-mode #'apheleia-global-mode #'auto-save-visited-mode #'blink-cursor-mode #'dtrt-indent-global-mode #'global-auto-revert-mode #'global-evil-mc-mode #'global-kkp-mode #'pcre-mode #'global-hide-mode-line-mode #'global-hl-todo-mode #'global-reveal-mode #'lost-selection-mode #'tool-bar-mode #'treemacs-follow-mode #'undelete-frame-mode #'scroll-bar-mode #'horizontal-scroll-bar-mode #'desktop-save-mode #'electric-layout-mode #'electric-quote-mode #'global-visual-line-mode #'global-emojify-mode #'editorconfig-mode #'magit-todos-mode #'magit-auto-revert-mode #'global-whitespace-mode
            ;; local
            #'+emacs-lisp--flycheck-non-package-mode #'+emacs-lisp-non-package-mode #'hl-todo-mode #'hl-line-mode #'akn/line-move-visual-mode #'akn/active-region-arrow-boundary-mode #'parinfer-rust-mode #'completion-preview-mode #'flycheck-popup-tip-mode #'flycheck-mode #'yas-minor-mode #'better-jumper-local-mode #'anzu-mode #'font-lock-mode #'whitespace-mode #'eldoc-mode #'prettify-symbols-mode #'evil-local-mode #'goto-address-mode #'ligature-mode #'subword-mode #'evil-snipe-local-mode #'evil-snipe-override-local-mode #'corfu-mode #'visual-line-mode #'adaptive-wrap-prefix-mode #'+word-wrap-mode #'undo-fu-session-mode #'tab-line-mode #'ws-butler-mode #'smartparens-mode #'diff-hl-mode #'symbol-overlay-mode #'pulsar-mode #'evil-surround-mode #'fish-completion-mode #'page-break-lines-mode #'akn/lisp-like-mode #'elisp-indent-docstrings-mode #'highlight-quoted-mode #'rainbow-delimiters-mode #'outline-minor-mode #'outli-mode #'highlight-numbers-mode #'display-line-numbers-mode #'goto-address-prog-mode #'envrc-mode #'auto-save-mode #'company-mode #'global-company-mode #'electric-operator-mode #'tree-sitter-mode #'tree-sitter-hl-mode #'treesit-fold-mode #'treesit-explore-mode #'treesit-inspect-mode #'treesit-fold-indicators-mode #'treesit-fold-line-comment-mode #'dtrt-indent-mode #'cua-mode #'git-auto-commit-mode #'hide-mode-line-mode #'indent-tabs-mode #'lispy-mode #'eglot-inlay-hints-mode #'blamer-mode #'smart-tabs-mode #'smartparens-strict-mode #'show-smartparens-mode #'global-superword-mode #'global-subword-mode #'ispell-minor-mode #'obsidian-mode #'hs-minor-mode #'ts-fold-mode #'superword-mode #'subword-mode #'flymake-mode #'so-long-minor-mode #'which-function-mode #'paragraph-indent-minor-mode #'reveal-mode #'view-mode #'vlf-mode #'text-scale-mode #'whitespace-newline-mode #'use-hard-newlines #'modifier-bar-mode #'reverse-im-mode #'eat-eshell-mode #'eat-eshell-visual-command-mode #'drag-stuff-mode #'completion-in-region-mode #'url-handler-mode #'treemacs-peek-mode #'global-display-line-numbers-mode #'treemacs-indent-guide-mode #'drag-stuff-global-mode #'temp-buffer-resize-mode
            ;; akn
            #'akn/new-file-mode #'akn/lisp-like-mode #'akn/lsp-minimal-mode #'akn/secret-paste-mode #'akn/shift-select-mode #'akn/indent-guides-mode #'akn/server-quit-mode #'akn/line-move-visual-mode #'akn/read-only-project-mode #'akn/enable-magit-delta-mode #'akn/magit-delta-global-mode #'akn/auto-commit-project-mode #'akn/tab-width-8-project-mode #'akn/github-explorer-file-mode #'akn/global-indent-guides-mode #'akn/obsidian-header-line-mode #'akn/turn-off-smartparens-mode #'akn/symbol-overlay-global-mode #'akn/auto-save-visited-local-mode #'akn/electric-operator-global-mode #'akn/recompile-on-save-project-mode #'akn/turn-off-auto-composition-mode #'akn/active-region-arrow-boundary-mode #'akn/disable-editorconfig-project-mode #'akn/local-smooth-scroll-disabled-mode #'akn/no-whitespace-butler-project-mode #'akn/evil-respect-line-move-visual-mode #'akn/immediate-auto-save-visited-local-mode #'akn/dont-require-final-newline-project-mode #'akn/active-region-arrow-boundary-global-mode #'akn/immediate-auto-save-visited-project-mode
            ;; doom
            #'doom-modeline-mode #'doom-docs-mode #'doom-debug-mode #'doom-big-font-mode #'+popup-mode #'+lua-love-mode #'+web-react-mode #'+word-wrap-mode #'+org-pretty-mode #'+transcribe-mode #'+web-django-mode #'+web-jekyll-mode #'+web-phaser-mode #'+popup-buffer-mode #'+tab-bar-show-mode #'+web-angularjs-mode #'+web-wordpress-mode #'+emacs-lisp-ert-mode #'+javascript-npm-mode #'+format-with-lsp-mode #'+javascript-gulp-mode #'+global-word-wrap-mode #'+lsp-optimization-mode #'+words/regularword-mode #'+mistty-strict-mouse-mode #'+vlf-auto-revert-tail-mode #'+emacs-lisp-non-package-mode #'+snippets/find-for-current-mode #'+tab-bar-show-in-minibuffer-mode #'+emacs-lisp--flymake-non-package-mode #'+emacs-lisp--flycheck-non-package-mode #'+multiple-cursors/mc-pause-cursors-mode)
  (pushnew! minions-demoted-modes
            #'evil-snipe-local-mode
            #'evil-snipe-override-local-mode
            #'evil-surround-mode
            #'override-global-mode
            #'anzu-mode
            ;; https://magit.vc/manual/magit/Legacy-Wip-Modes.html
            #'magit-wip-initial-backup-mode #'magit-wip-before-change-mode #'magit-wip-after-apply-mode #'magit-wip-after-save-mode #'magit-wip-after-save-local-mode))
