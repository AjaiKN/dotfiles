;;; lang/jsonian/autoload/jsonian-lines-mode.el -*- lexical-binding: t; -*-

(require 'jsonian)

;;;###autoload
(define-derived-mode +jsonian-lines-mode jsonian-mode "JSONL"
  "A major mode for editing JSON Lines (JSONL) files."
  :group 'akn)
