;;; completion/preview/autoload/history-capf.el -*- lexical-binding: t; -*-

;; from https://github.com/emacsmirror/capf-autosuggest/blob/master/capf-autosuggest.el

;;;###autoload
(defun +preview-history-capf ()
  "Completion-at-point function for history.
Supports `comint-mode', `eshell-mode' and the minibuffer.  In
comint end eshell, it is applicable only if point is after the
last prompt."
  (cond
   ((derived-mode-p 'comint-mode)
    (+preview-comint-capf))
   ((derived-mode-p 'eshell-mode)
    (+preview-eshell-capf))
   ((minibufferp)
    (+preview-minibuffer-capf))))

(defun +preview-comint-capf ()
  "Completion-at-point function for comint input history.
Is only applicable if point is after the last prompt."
  (let ((ring comint-input-ring)
        (beg nil) (end nil))
    (or (and (setq beg comint-accum-marker)
             (setq beg (marker-position beg)))
        (and (setq beg (get-buffer-process (current-buffer)))
             (setq beg (marker-position (process-mark beg)))))
    (cond
     ;; Return nil to allow possible further capf functions
     ((null beg) nil)
     ((< (point) beg) nil)
     ((<= (setq end (if comint-use-prompt-regexp
                        (line-end-position)
                      (field-end)))
          beg)
      ;; Return non-nil but empty completion table to block possible further
      ;; capf functions
      (list (point) (point) nil))
     ((and (ring-p ring) (not (ring-empty-p ring)))
      (list beg end (+preview--completion-table ring)
            :exclusive 'no)))))

(defun +preview-eshell-capf ()
  "Completion-at-point function for eshell input history.
Is only applicable if point is after the last prompt."
  (let ((ring eshell-history-ring)
        (beg (save-excursion (beginning-of-line) (point)))
        (end (point-max)))
    (cond
     ((< (point) eshell-last-output-end) nil)
     ((< (point) beg) nil)
     ((and (= end beg) (eshell-interactive-process))
      (list (point) (point) nil))
     ((and (ring-p ring) (not (ring-empty-p ring)))
      (list beg end (+preview--completion-table ring)
            :exclusive 'no)))))

(defun +preview--completion-table (ring)
  "Return a completion table to complete on RING."
  (let ((ring-elems t))
    (lambda (input predicate action)
      (when (eq ring-elems t)
        (setq ring-elems (ring-elements ring)))
      (complete-with-action action ring-elems input predicate))))

(defun +preview-minibuffer-capf ()
  "Completion-at-point function for minibuffer history."
  (let ((hist minibuffer-history-variable)
        (should-prin1 nil))
    (when (and (not (eq t hist))
               (setq hist (symbol-value hist)))
      (when (eq minibuffer-history-sexp-flag (minibuffer-depth))
        (setq should-prin1 t))
      (list (minibuffer-prompt-end)
            (point-max)
            (if should-prin1
                (lambda (input predicate action)
                  (when should-prin1
                    (setq hist (mapcar #'prin1-to-string hist)
                          should-prin1 nil))
                  (complete-with-action action hist input predicate))
              hist)
            :exclusive 'no))))
