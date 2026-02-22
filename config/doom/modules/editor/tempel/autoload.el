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
