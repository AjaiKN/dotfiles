;;; lang/log/config.el -*- lexical-binding: t; -*-

(defvar +log-mode-keywords
  '(("\\<[1-3][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]T?\\>" . font-lock-constant-face)
    ("\\<[0-9]+:[0-5][0-9]\\(:[0-5][0-9]\\)?\\(\\.[0-9]+\\)?" . font-lock-constant-face)
    "\\<[[:upper:]_]+\\([0-9]+[[:upper:]_]*\\)*\\>"
    ("\\<\\(CRIT\\|CRITICAL\\|EMERG\\|EMERGENCY\\|ERROR\\|FATAL\\|PANIC\\|Error\\|error\\)\\>" . 'error)
    ("\\<\\(\\|WARN\\|WARNING\\|Warning\\|warning\\|Warn\\|ALERT\\|DEBUG\\|HEAVYDEBUG\\|INFO\\|NOTICE\\|TRACE\\|TRACEDEBUG\\)\\>" . font-lock-warning-face)
    ("^[[:space:][:punct:]]*[[:alpha:]]: " . font-lock-builtin-face)
    ("^[[:space:]!@#$%^&*-=+|\\\\;:<>.?]+" . font-lock-builtin-face)
    ("[][(){}]" . 'font-lock-delimiter-face)
    ("\\<[0-9]+\\([,.][0-9]+\\)*" . 'font-lock-number-face)
    ("[[:punct:]]" . 'font-lock-misc-punctuation-face))
  "The `font-lock-keywords' for `+log-mode'.")

(defvar +log-mode-defaults
  '(+log-mode-keywords
    keywords-only
    nil)
  "The `font-lock-defaults' for `+log-mode'.")

(define-derived-mode +log-mode prog-mode "Log"
  "A generic major mode for logs."
  :group 'akn
  (setq-local buffer-read-only t)
  (view-mode)
  (if (bound-and-true-p vlf-mode)
      (when (modulep! :emacs vlf)
        (+vlf-auto-revert-tail-mode))
    (auto-revert-tail-mode))
  (setq-local font-lock-defaults +log-mode-defaults)
  (so-long-minor-mode))

(map! :map +log-mode-map
      :localleader
      "h" (akn/cmds! (use-region-p) #'highlight-regexp #'highlight-symbol-at-point))

(add-to-list 'auto-mode-alist '("\\.log\\'" . +log-mode))

;; Also see:
;; https://writequit.org/articles/working-with-logs-in-emacs.html
;; https://github.com/doublep/logview
;; https://github.com/ananthakumaran/rails-log-mode
;; https://github.com/vapniks/syslog-mode
