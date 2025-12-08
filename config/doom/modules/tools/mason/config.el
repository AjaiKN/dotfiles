;;; tools/mason/config.el -*- lexical-binding: t; -*-

;; Add Mason's bin directory to PATH right away.
(defvar mason-dir (expand-file-name "mason" doom-cache-dir))
(let ((bin-dir (expand-file-name "bin" mason-dir)))
  (setenv "PATH" (concat bin-dir ":" (getenv "PATH")))
  (add-to-list 'exec-path bin-dir))

(use-package! mason
  :hook (doom-first-input . mason-ensure))
