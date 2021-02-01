;;; editor/multiple-cursors/autoload/mc.el -*- lexical-binding: t; -*-

(require 'multiple-cursors)
(require 'akn)

;;;###autoload
(defun +multiple-cursors/mc-make-cursor-move-next-line (arg)
  (interactive "p")
  (let ((line-move-visual nil))
    ;; (let ((mark-active nil))
    ;;   (mc/mark-next-like-this (or arg 1))))
    (deactivate-mark)
    (dotimes (_ arg)
      (mc/create-fake-cursor-at-point)
      (when-let* ((furthest-cursor (mc/furthest-cursor-after-point))
                  (furthest-point (overlay-get furthest-cursor 'point)))
        (goto-char furthest-point))
      (next-line))
    (mc/maybe-multiple-cursors-mode)))

;;;###autoload
(defun +multiple-cursors/mc-make-cursor-move-prev-line (arg)
  (interactive "p")
  (let ((line-move-visual nil))
    ;; (let ((mark-active nil))
    ;;   (mc/mark-previous-like-this (or arg 1)))
    (deactivate-mark)
    (dotimes (_ arg)
      (mc/create-fake-cursor-at-point)
      (when-let* ((furthest-cursor (mc/furthest-cursor-before-point))
                  (furthest-point (overlay-get furthest-cursor 'point)))
        (goto-char furthest-point))
      (previous-line))
    (mc/maybe-multiple-cursors-mode)))

;;;###autoload
(define-minor-mode +multiple-cursors/mc-pause-cursors-mode
  "Freezes/unfreezes the fake cursors, just like
`evil-mc-pause-cursors' / `+multiple-cursors/evil-mc-toggle-cursors'."
  :group 'akn
  (when multiple-cursors-mode
    ;; See `multiple-cursors-mode'
    (if +multiple-cursors/mc-pause-cursors-mode
        (progn
          (akn/remove-hook 'pre-command-hook 'mc/make-a-note-of-the-command-being-run nil t)
          (akn/remove-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t t))
      (add-hook 'pre-command-hook 'mc/make-a-note-of-the-command-being-run nil t)
      (add-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t t))))

;;;###autoload
(defun +multiple-cursors/mc-toggle-cursors ()
  "See `+multiple-cursors/evil-mc-toggle-cursors'"
  (interactive)
  (+multiple-cursors/mc-pause-cursors-mode 'toggle))

;;;###autoload
(defun +multiple-cursors/mc-toggle-cursor-here ()
  "See `+multiple-cursors/evil-mc-toggle-cursor-here'"
  (interactive)
  (mc/create-fake-cursor-at-point)
  (mc/maybe-multiple-cursors-mode)
  (+multiple-cursors/mc-pause-cursors-mode))
