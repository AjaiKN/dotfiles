;;; lang/sage/config.el -*- lexical-binding: t; -*-

(use-package! sage-shell-mode
  :defer t
  :init
  ;; also see `sage-shell:define-alias'
  (defalias 'akn/run-sage  #'sage-shell:run-sage)
  (defalias 'akn/sage-repl #'sage-shell:run-sage)
  (defalias 'akn/run-new-sage  #'sage-shell:run-new-sage)
  (defalias 'akn/sage-repl-new #'sage-shell:run-new-sage)
  :config
  (setq sage-shell:input-history-cache-file
        (concat doom-cache-dir "sage_shell_input_history")))

