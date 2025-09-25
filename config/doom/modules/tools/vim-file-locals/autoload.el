;;; tools/vim-file-locals/autoload.el -*- lexical-binding: t; -*-

(defvar +vim-file-locals--shift-width-options
  '("shiftwidth" "sw")
  "The vim options corresponding to `evil-shift-width' (and usually
`tab-width' too).")
(defvar +vim-file-locals--tab-width-options
  '("tabstop" "ts" "softtabstop" "sts")
  "The vim options corresponding to `tab-width'.")
(defvar +vim-file-locals--indent-tabs-mode-options
  '("expandtab" "et" "noexpandtab" "noet")
  "The vim options corresponding to `indent-tabs-mode'.")

(defun +vim-file-locals--get-options (allowed-option-aliases)
  (cl-loop for (key . val) in vim-file-locals-buffer-options
           thereis (and (member key allowed-option-aliases) val)))

;;;###autoload
(defun +vim-file-locals--tab-width-stuff-h ()
  (let ((sw (+vim-file-locals--get-options +vim-file-locals--shift-width-options))
        (ts (+vim-file-locals--get-options +vim-file-locals--tab-width-options))
        (et (+vim-file-locals--get-options +vim-file-locals--indent-tabs-mode-options)))
    ;; If any of these indentation-related things are set, don't try to
    ;; auto-detect indentation with dtrt-indent.
    (when (or ts sw et)
      (setq-local +whitespace-guess-inhibit 'vim-file-locals))
    ;; If it specifies that tab-width and shift-width are different, let them be
    ;; different.
    (when (and ts sw (not (equal ts sw)))
      (setq-local akn/evil-shift-width-different-from-tab-width t)
      ;; TODO: PR
      ;; HACK: I think vim-file-locals has these two mixed up?
      (setq-local tab-width evil-shift-width
                  evil-shift-width tab-width))))
