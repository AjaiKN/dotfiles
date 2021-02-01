;;; editor/multiple-cursors/autoload/switching.el -*- lexical-binding: t; -*-

;;;; multiple-cursors
(defun +multiple-cursors--mc-fake-cursor-positions ()
  "Return the positions of the `multiple-cursors' fake cursors."
  (mapcar #'overlay-start (mc/all-fake-cursors)))

(defun +multiple-cursors--mc-create-cursors-at (positions)
  "Create `multiple-cursors' cursors at POSITIONS."
  (dolist (pos positions)
    (save-excursion
      (goto-char pos)
      (mc/create-fake-cursor-at-point)))
  (mc/maybe-multiple-cursors-mode))

;;;; evil-mc

(defun +multiple-cursors--evil-mc-fake-cursor-positions ()
  "Return the positions of the `evil-mc' fake cursors."
  (require 'evil-mc)
  (mapcar #'evil-mc-get-cursor-start evil-mc-cursor-list))

(defun +multiple-cursors--evil-mc-create-cursors-at (positions)
  "Create `evil-mc' cursors at POSITIONS."
  (require 'evil-mc)
  (evil-mc-run-cursors-before)
  (dolist (pos positions)
    (evil-mc-make-cursor-at-pos pos)))

;;;; switching between evil-mc and multiple-cursors

;;;###autoload
(defun +multiple-cursors/evil-mc->mc ()
  "Switch from `evil-mc' to `multiple-cursors', recreating fake cursors."
  (interactive)
  (when (and (not (derived-mode-p 'minibuffer-mode))
             (bound-and-true-p evil-mc-mode)
             (not (memq this-command '(evil-change evil-change-line evil-change-whole-line))))
    (require 'evil-mc)
    (require 'multiple-cursors)
    (let ((positions (+multiple-cursors--evil-mc-fake-cursor-positions)))
      (when (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
        (evil-mc-resume-cursors))
      (evil-mc-mode -1)
      (+multiple-cursors--mc-create-cursors-at positions)
      (mc/execute-command-for-all-cursors (cmd! (deactivate-mark))))))

;;;###autoload
(defun +multiple-cursors/mc->evil-mc ()
  "Switch from `multiple-cursors' to `evil-mc', recreating fake cursors."
  (interactive)
  (when (bound-and-true-p rectangular-region-mode)
    (require 'rectangular-region-mode)
    (rrm/switch-to-multiple-cursors))
  (when (and (not (derived-mode-p 'minibuffer-mode))
             (bound-and-true-p multiple-cursors-mode))
    (require 'evil-mc)
    (require 'multiple-cursors)
    (when (and (memq this-command (list #'evil-normal-state #'evil-escape))
               evil-move-cursor-back)
      (mc/execute-command-for-all-fake-cursors
       (cmd! (unless (or (eolp) (bolp)) (backward-char)))))
    (let ((positions (+multiple-cursors--mc-fake-cursor-positions)))
      (multiple-cursors-mode -1)
      (deactivate-mark)
      (+multiple-cursors--evil-mc-create-cursors-at positions))))

;;;###autoload
(defun +multiple-cursors/mc-toggle-evil ()
  "Switch from `multiple-cursors-mode' if in `evil-mc-mode', or vice versa."
  (interactive)
  (cond
   ((or (bound-and-true-p multiple-cursors-mode)
        (bound-and-true-p rectangular-region-mode))
    (+multiple-cursors/mc->evil-mc))
   ((bound-and-true-p evil-mc-mode) (+multiple-cursors/evil-mc->mc))))

;;;###autoload
(defun +multiple-cursors-exit (&rest _)
  (when (bound-and-true-p rectangular-region-mode)
    (require 'rectangular-region-mode)
    (rrm/switch-to-multiple-cursors))
  (when (or (bound-and-true-p evil-mc-mode)
            (bound-and-true-p multiple-cursors-mode))
    (when (bound-and-true-p evil-mc-mode)
      (require 'evil-mc)
      (when (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
        (evil-mc-resume-cursors))
      (evil-mc-mode -1))
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1)
      (deactivate-mark))
    t))

;;;###autoload
(defun +multiple-cursors/exit ()
  (interactive)
  (+multiple-cursors-exit))
