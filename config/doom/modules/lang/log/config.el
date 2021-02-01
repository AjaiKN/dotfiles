;;; lang/log/config.el -*- lexical-binding: t; -*-

(define-derived-mode +log-mode prog-mode "Log"
  "A generic major mode for logs."
  :group 'akn
  (setq-local buffer-read-only t)
  (view-mode)
  (if (bound-and-true-p vlf-mode)
      (when (modulep! :emacs vlf)
        (+vlf-auto-revert-tail-mode))
    (auto-revert-tail-mode))
  (so-long-minor-mode))

(add-to-list 'auto-mode-alist '("\\.log\\'" . +log-mode))

;; Also see:
;; https://writequit.org/articles/working-with-logs-in-emacs.html
;; https://github.com/doublep/logview
;; https://github.com/ananthakumaran/rails-log-mode
;; https://github.com/vapniks/syslog-mode
