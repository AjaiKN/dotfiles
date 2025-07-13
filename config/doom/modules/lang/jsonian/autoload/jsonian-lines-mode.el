;;; lang/jsonian/autoload/jsonian-lines-mode.el -*- lexical-binding: t; -*-

(require 'jsonian)

;;;###autoload
(define-derived-mode +jsonian-lines-mode jsonian-mode "JSONL"
  "A major mode for editing JSON Lines (JSONL) files."
  :group 'akn)

(map! :map +jsonian-lines-mode-map
      :localleader
      "v" #'+jsonian-lines/view-record)

(defun +jsonian-lines/view-record ()
  (interactive nil +jsonian-lines-mode)
  (let* ((full-buf (current-buffer))
         (full-buf-name (buffer-name full-buf))
         (line-num (line-number-at-pos))
         (line-start (pos-bol))
         (line-end (pos-eol))
         (new-buf (get-buffer-create (format "%s: record %d" full-buf-name line-num)))
         (inhibit-read-only t))
    (with-current-buffer new-buf
      (erase-buffer)
      (insert-buffer-substring full-buf line-start line-end)
      (jsonian-mode)
      (jsonian-format-region (point-min) (point-max))
      (setq buffer-read-only t))
    ;; (pop-to-buffer new-buf (cons #'display-buffer-in-side-window '((side . right))))
    (+popup-buffer new-buf '((side . right)))))
(defalias '+jsonian-lines/view-line #'+jsonian-lines/view-record)
