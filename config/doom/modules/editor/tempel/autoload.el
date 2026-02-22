;;; editor/tempel/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tempel-setup-capf-h ()
  (push #'tempel-complete completion-at-point-functions))

;;;###autoload
(defun +tempel-hippie-try-expand (old)
  "Integrate tempel with hippie-expand.

Put this in `hippie-expand-try-functions-list'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (condition-case _err
          (tempel-expand t)
        (user-error nil))
    (undo 1)
    nil))

;;;###autoload
(defun +tempel-add-user-elements (elt _fields)
 (pcase elt
   ;; include another template
   (`(i ,inc)
    (cons 'l (or (alist-get inc (tempel--templates))
                 (error "Template %s not found" inc))))
   (`(indent . ,things)
    (let ((beginning (point-marker)))
      `(l
        ,@things
        (ignore
         (if (eq (marker-buffer ,beginning) (current-buffer))
             (progn
               (unless (bound-and-true-p smart-tabs-mode)
                 (funcall (if indent-tabs-mode #'tabify #'untabify)
                          (marker-position ,beginning) (point)))
               (indent-region (marker-position ,beginning) (point)))
           (error "+tempel-add-user-elements: indent: marker isn't in this buffer"))))))))

;;;###autoload
(defun +tempel-active-p ()
  (bound-and-true-p tempel--active))

(defun +tempel--parse-yasnippet-template (str)
  (car (+tempel--parse-yasnippet-template-1 str)))

;; concatenate consecutive strings, remove empty strings, and reverse
(defun +tempel--simplify-yasnippet-template (thing)
  (cl-loop with ret = nil
           for el in thing
           when (and el (not (equal el "")))
           do (if (and (stringp (car ret)) (stringp el))
                  (setf (car ret) (concat (car ret) el))
                (push el ret))
           finally return ret))

(defun +tempel--read-sexp (&optional start)
  (save-excursion
    (goto-char (or start (point)))
    (when-let* ((sexp (ignore-errors (read (current-buffer)))))
      (cons sexp (point)))))

(defun +tempel--parse-yasnippet-template-1 (str &optional nested-p)
  (require 'yasnippet)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((contents nil)
            (transformer nil))
        (while (search-forward-regexp (rx (any "`$\\")) nil 'noerror)
          (pcase (char-before)
            (?\\ (when (memq (char-after (1+ (point))) '(?\{ ?\} ?\( ?\) ?\\ ?\` ?\" ?\' ?\$))
                   (delete-char -1)
                   (forward-char 1)))
            (?\$ (backward-char)
                 (cond ((looking-at "$\\([0-9]+\\)") ; e.g. $12
                        (push (buffer-substring-no-properties (point-min) (point)) contents)
                        (push (list 'yas-field (string-to-number (match-string 1))) contents)
                        (delete-region (point-min) (match-end 0)))
                       ((looking-at yas--field-regexp)
                        (push (buffer-substring-no-properties (point-min) (point)) contents)
                        (let* ((brace-scan (save-match-data
                                             (goto-char (match-beginning 2))
                                             (yas--scan-for-field-end)))
                               (closing-brace 125)
                               (real-match-end-0 (if (eq closing-brace (char-before brace-scan))
                                                     brace-scan
                                                   (point)))
                               (number (and (match-string-no-properties 1)
                                            (string-to-number (match-string-no-properties 1))))
                               (field2 (match-string-no-properties 2)))
                          (push (cond
                                 ;; e.g. ${12}
                                 ((and (not number) (string-match-p "\\`[0-9]+\\'" field2))
                                  (list 'yas-field (string-to-number field2)))
                                 ;; e.g. ${2:$(capitalize yas-text)}
                                 ((and-let* (((string-match-p "\\`\\$[ \t\n]*(" field2))
                                             (sexp+end (+tempel--read-sexp (1+ (match-beginning 2))))
                                             (sexp (car sexp+end))
                                             (end (cdr sexp+end)))
                                    (list 'yas-mirror number sexp)))
                                 ;; e.g. ${1:something}
                                 (t
                                  `(yas-field
                                    ,number
                                    ,@(+tempel--parse-yasnippet-template-1
                                       (buffer-substring-no-properties (match-beginning 2)
                                                                       (1- real-match-end-0))
                                       'nested))))
                                contents)
                          (delete-region (point-min) real-match-end-0)
                          (goto-char (point-min))))
                       ;; e.g. $(upcase yas-text)
                       ((save-excursion
                          (and-let* (((and nested-p (null transformer)))
                                     ((looking-at (if (and (bobp) (null contents))
                                                      ;; e.g. ${1:mydefine$(upcase yas-text)}
                                                      "\\$\\$[ \t\n]*\\(([^)]*)\\)"
                                                    ;; e.g. ${1:$$(upcase yas-text)}
                                                    "\\$[ \t\n]*\\(([^)]*)\\)")))
                                     (sexp+end (+tempel--read-sexp (match-beginning 1)))
                                     (sexp (car sexp+end))
                                     (end (cdr sexp+end)))
                            (push (buffer-substring-no-properties (point-min) (point)) contents)
                            (setq transformer sexp)
                            (delete-region (point-min) end)
                            t)))
                       ;; e.g. $>
                       ((looking-at (rx "$>"))
                        (push (buffer-substring-no-properties (point-min) (point)) contents)
                        (push 'yas-indent contents)
                        (delete-region (point-min) (match-end 0)))
                       (t
                        (forward-char))))
            (?\` (when-let* ((beg (point))
                             (sexp+end (+tempel--read-sexp))
                             (sexp (car sexp+end))
                             (end (cdr sexp+end))
                             (end-char (char-after end))
                             ((eq end-char ?\`)))
                   (push (buffer-substring-no-properties (point-min) (1- (point))) contents)
                   (delete-region (point-min) (1+ end))
                   (push (list 'yas-subst sexp) contents)))))
        (push (buffer-string) contents)
        (list (+tempel--simplify-yasnippet-template contents) transformer)))))
