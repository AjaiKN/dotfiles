;;; editor/tempel/autoload.el -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'akn))

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
(defun +tempel-add-user-elements (elt fields)
 (pcase elt
   ;; add q with arguments
   ;; TODO: PR
   (`(q . ,rest)
    (let ((ov (apply #'tempel--placeholder rest)))
      (overlay-put ov 'tempel--enter #'tempel--done)))
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
           (error "+tempel-add-user-elements: indent: marker isn't in this buffer"))))))
   (`(yas ,template)
    (cons 'l (+tempel--parse-yasnippet-template template)))
   ((or `(yas-field ,n)
        `(yas-field ,n ,contents)
        `(yas-field ,n ,contents ,transformer))
    (when transformer (error "field transformer not supported yet"))
    (let ((field-name (intern (format "yas-field-%s" n))))
      (cond
       ((and (assq field-name fields)
             (null contents)
             (null transformer))
        field-name)
       (t
        (list (if (eq n 0) 'q 'p)
              `(+tempel--subst (+tempel--concat ,@contents))
               field-name)))))
   (`(yas-mirror ,n ,transformer)
    (let ((field-name (intern (format "yas-field-%s" n))))
      `(dlet ((yas-text (or ,field-name "")))
         (or ,transformer ""))))
   (`(yas-subst ,expr)
    `(+tempel--subst
       ,expr))))

(defmacro +tempel--subst (expr)
  `(akn/letf! ((#'yas-subst #'+tempel--subst))
     ,expr))

(defun +tempel--concat (&rest stuff)
  (cl-loop for thing in stuff
           when thing
           concat thing))

;;;###autoload
(defun +tempel-active-p ()
  (bound-and-true-p tempel--active))

(defun +tempel--read-yasnippet-directives ()
  "Return an alist of yasnippet directives at the top of the current buffer."
  (goto-char (point-min))
  (when (re-search-forward "^# --\\s-*\n" nil 'noerror)
    (let ((end-of-headers (point)))
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*?\\)[[:space:]]*$"
                                          end-of-headers
                                          'noerror)
                 collect (cons (match-string-no-properties 1)
                               (match-string-no-properties 2)))))))

(defun +tempel--parse-yasnippet-template (str)
  (car (+tempel--parse-yasnippet-template-1 str)))

;; concatenate consecutive strings, remove empty strings, and reverse
(defun +tempel--simplify-yasnippet-template (thing)
  (cl-loop with ret = nil
           for el in thing
           when (and el (not (equal el "")))
           do (if (and (stringp (car ret)) (stringp el))
                  (setf (car ret) (concat el (car ret)))
                (push el ret))
           finally return ret))

(defun +tempel--read-sexp (&optional start)
  (save-match-data
    (let ((orig (point)))
      (goto-char (or start (point)))
      (or
       (ignore-errors
         (read (current-buffer)))
       (progn
         (goto-char orig)
         nil)))))

(cl-defun +tempel--yas--scan-for-field-end ()
  (save-match-data
    (while (progn (or (re-search-forward "\\${\\|}" nil 'noerror)
                      (cl-return nil))
                  (when (eq (char-before) ?\{)
                    ;; Nested field.
                    (+tempel--yas--scan-for-field-end))))
    t))

(defun +tempel--parse-yasnippet-template-1 (str &optional nested-p)
  (require 'yasnippet)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((contents nil)
            (transformer nil))
        (while (search-forward-regexp (rx (any "`$\\")) nil 'noerror)
          (unless (eq (point) 2)
            (push (delete-and-extract-region (point-min) (1- (point))) contents))
          (pcase (char-before)
            (?\\ (when (memq (char-after) '(?\{ ?\} ?\( ?\) ?\\ ?\` ?\" ?\' ?\$))
                   (delete-char -1)
                   (forward-char 1)))
            (?\$ (cond ((or (looking-at "\\([0-9]+\\)")    ; e.g. $12
                            (looking-at "{\\([0-9]+\\)}")) ; e.g. ${12}
                        (push (list 'yas-field (string-to-number (match-string 1))) contents)
                        (delete-region (point-min) (match-end 0)))
                       ((save-excursion
                          (and-let* (((when (looking-at "{\\([0-9]+\\):")
                                        (goto-char (match-end 0))
                                        t))
                                     (number (string-to-number (match-string-no-properties 1)))
                                     (field2-beg (point))
                                     ((+tempel--yas--scan-for-field-end))
                                     (field2 (buffer-substring-no-properties field2-beg (1- (point)))))
                            (push (if-let* (((string-match-p "\\`\\$[ \t\n]*(" field2))
                                            (sexp (+tempel--read-sexp (1+ field2-beg))))
                                      (progn
                                        (forward-char)
                                        ;; e.g. ${2:$(capitalize yas-text)}
                                        `(yas-mirror ,number ,sexp))
                                    ;; e.g. ${1:something}
                                    `(yas-field
                                      ,number
                                      ,@(+tempel--parse-yasnippet-template-1
                                         (buffer-substring-no-properties field2-beg (1- (point)))
                                         'nested)))
                                  contents)
                            (delete-region (point-min) (point))
                            t)))
                       ;; e.g. $(upcase yas-text)
                       ((save-excursion
                          (and-let* (((and nested-p (null transformer)))
                                     ((looking-at (if (null contents)
                                                      ;; e.g. ${1:mydefine$(upcase yas-text)}
                                                      "\\$[ \t\n]*\\(([^)]*)\\)"
                                                    ;; e.g. ${1:$$(upcase yas-text)}
                                                    "[ \t\n]*\\(([^)]*)\\)")))
                                     (sexp (+tempel--read-sexp (match-beginning 1))))
                            (setq transformer sexp)
                            (delete-region (point-min) (point))
                            t)))
                       ;; e.g. $>
                       ((looking-at (rx ">"))
                        (push 'yas-indent contents)
                        (delete-region (point-min) (match-end 0)))))
            (?\` (when-let* ((sexp (+tempel--read-sexp))
                             ((eq (char-after) ?\`)))
                   (forward-char)
                   (push `(yas-subst ,sexp) contents)
                   (delete-region (point-min) (point))))))
        (push (buffer-substring-no-properties (point-min) (point-max)) contents)
        (list (+tempel--simplify-yasnippet-template contents) transformer)))))
