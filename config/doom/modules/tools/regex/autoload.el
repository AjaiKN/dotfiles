;;; tools/regex/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +regex/xr-at-point (&optional should-replace should-message)
  (interactive (list current-prefix-arg t))
  (let* ((start
          (progn
            ;; http://xahlee.info/emacs/emacs/elisp_determine_cursor_inside_string_or_comment.html
            (if (nth 3 (syntax-ppss))
                (goto-char (nth 8 (syntax-ppss)))
              (while (and (not (eolp))
                          (not (eq (char-after) ?\")))
                (forward-char)))
            (point)))
         (str (read (current-buffer)))
         (_err (when (not (stringp str))
                 (user-error "current string not found")))
         (end (point))
         (xr-inner (xr str +regex-xr-dialect))
         (xr-expr (if (and (listp xr-inner)
                           (memq (car xr-inner) '(seq :)))
                      (cons 'rx (cdr xr-inner))
                    (list 'rx xr-inner)))
         (pretty-printed (string-trim (xr-pp-rx-to-str xr-expr)))
         (pretty-printed-one-line (replace-regexp-in-string (rx (? "\r") "\n" (* space)) " " pretty-printed)))
    (when should-replace
      (goto-char start)
      (kill-region start end)
      (insert pretty-printed-one-line))
    (when should-message
      (message "%s" pretty-printed))
    xr-expr))

;;;###autoload
(defun +regex/xr-replace-at-point (&optional should-message)
  (interactive current-prefix-arg)
  (+regex/xr-at-point t should-message))

;;;###autoload
(defun +regex/xr-try-replace-at-point (&optional should-message)
  (interactive current-prefix-arg)
  (+regex/xr-at-point (not buffer-read-only) (or buffer-read-only should-message)))
