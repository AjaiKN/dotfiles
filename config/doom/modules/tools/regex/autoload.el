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

;;;###autoload
(defmacro prx (&rest expressions)
  "Convert the rx-compatible regular EXPRESSIONS to PCRE.
Most shell applications accept Perl Compatible Regular Expressions.

From https://howardism.org/Technical/Emacs/eshell-why.html and
https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#regular-expressions (CC0)."
  `(rx-let ((integer (1+ digit))
            (float   (seq integer "." integer))
            (b256    (seq (optional (or "1" "2"))
                          (regexp "[0-9]\\{1,2\\}")))
            (ipaddr  (seq b256 "." b256 "." b256 "." b256))
            (time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
            (email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
            (date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
            (ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
            (uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
            (guid    (seq uuid)))
     (rxt-elisp-to-pcre (rx ,@expressions))))
