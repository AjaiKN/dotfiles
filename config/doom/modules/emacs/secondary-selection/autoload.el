;;; emacs/secondary-selection/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun akn/remove-secondary-selection ()
  (when (secondary-selection-exist-p)
    (delete-overlay mouse-secondary-overlay)
    t))

;;;###autoload
(defun akn/secondary-selection ()
  (gui-get-selection 'SECONDARY))

;;;###autoload
(defun akn/set-secondary-selection (beg end)
  (delete-overlay mouse-secondary-overlay) ; Delete the secondary selection even on a different buffer.
  (move-overlay mouse-secondary-overlay beg end))
;;;###autoload
(defun akn/set-region (beg end)
  (when (or beg end)
    (set-mark  (or beg end))
    (goto-char (or end beg))
    (activate-mark)))

;;;###autoload
(defmacro akn/save-secondary-selection (&rest body)
  (declare (indent defun))
  (cl-with-gensyms (beg end)
    `(if (secondary-selection-exist-p)
         (let ((,beg (overlay-start mouse-secondary-overlay))
               (,end (overlay-end mouse-secondary-overlay)))
           (unwind-protect
               (progn ,@body)
             (akn/set-secondary-selection ,beg ,end)))
       ,@body
       (delete-overlay mouse-secondary-overlay))))
;;;###autoload
(defmacro akn/save-mark-and-excursion-and-secondary-selection (&rest body)
  (declare (indent defun))
  `(akn/save-secondary-selection
     (save-mark-and-excursion
       ,@body)))

;;;###autoload
(defun akn/insert-secondary-selection ()
  (interactive)
  (let ((mouse-yank-at-point t))
    (mouse-yank-secondary nil)))

;;;###autoload
(defun akn/set-secondary-selection-to-region ()
  (interactive)
  (secondary-selection-from-region))

;;;###autoload
(defun akn/set-region-to-secondary-selection ()
  (interactive)
  (secondary-selection-to-region))
(defmacro akn/with-secondary-selection-as-region (&rest body)
  (declare (indent defun))
  `(akn/save-mark-and-excursion-and-secondary-selection
     (akn/set-region-to-secondary-selection)
     ,@body))

;;;###autoload
(defun akn/copy-secondary-selection ()
  (interactive)
  (akn/with-secondary-selection-as-region
    (kill-ring-save (mark) (point))
    ;; save secondary selection in register s
    (set-register ?s (akn/region-string)))
  (message "Copied"))

;;;###autoload
(defun akn/swap-region-and-secondary-selection ()
  (interactive)
  (let ((region-beg (region-beginning))
        (region-end (region-end))
        (secondary-exists-p (secondary-selection-exist-p))
        (secondary-beg (overlay-start mouse-secondary-overlay))
        (secondary-end (overlay-end mouse-secondary-overlay)))
    (when (and (modulep! :editor evil) (evil-normal-state-p) secondary-beg secondary-end (< secondary-beg secondary-end))
      (cl-decf secondary-end))
    (if (region-active-p)
        (akn/set-secondary-selection region-beg region-end)
      (akn/set-secondary-selection (point) (point)))
    (cond
     ((and (equal secondary-beg secondary-end) secondary-beg)
      (progn (goto-char secondary-beg)
             (deactivate-mark)))
     (secondary-exists-p (akn/set-region secondary-beg secondary-end))
     (t (deactivate-mark)))))
