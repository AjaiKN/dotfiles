;;; tools/guix/config.el -*- lexical-binding: t; -*-

(defun +guix/attach-buffer-to-repl (&optional buffer repl)
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (require 'guix-repl)
  (or repl (setq repl (guix-get-repl-buffer)))
  (setq-local geiser-impl--implementation nil)
  (geiser-repl--set-this-buffer-repl repl buffer))
