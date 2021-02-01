;;; tools/regex/config.el -*- lexical-binding: t; -*-

(defcustom +regex-xr-dialect 'medium
  "See the dialect argument of `xr'."
  :group 'akn
  :type '(choice
          (const :tag "verbose: verbose keywords" verbose)
          (const :tag "medium: somewhat verbose keywords (the default)" medium)
          (const :tag "brief: short keywords" brief)
          (const :tag "terse: very short keywords" terse)))
(setq! +regex-xr-dialect 'terse)

;; https://www.spacemacs.org/doc/DOCUMENTATION.html#regular-expressions
(use-package! pcre2el
  :defer t
  :init
  (defvar akn/pcre2el-keymap (make-sparse-keymap))
  (map! :map akn/pcre2el-keymap
        "/"   #'rxt-explain
        "c"   #'rxt-convert-syntax
        ;; "x"   #'rxt-convert-to-rx
        "x"   #'rxt-explain
        "'"   #'rxt-convert-to-strings
        "p /" #'rxt-explain-pcre
        "p e" #'rxt-pcre-to-elisp
        "p x" #'rxt-pcre-to-rx
        "p '" #'rxt-pcre-to-strings
        "e /" #'rxt-explain-elisp
        "e p" #'rxt-elisp-to-pcre
        "e x" #'rxt-elisp-to-rx
        "e '" #'rxt-elisp-to-strings
        "e t" #'rxt-toggle-elisp-rx
        "t"   #'rxt-toggle-elisp-rx
        "%"   #'pcre-query-replace-regexp)
  :config
  (set-popup-rule! (rx "Regexp Explain")))
