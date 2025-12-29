;;; autoload/dired.el -*- lexical-binding: t; -*-

;;; Helpers

;;;###autoload
(defun akn/dirvish-side-p ()
  (and-let* (((derived-mode-p 'dired-mode))
             ((featurep 'dirvish))
             (curr (dirvish-curr)))
    (eq 'side (dv-type curr))))

;;;###autoload
(defun akn/dired-directory-p ()
  "Is point currently on a directory (as opposed to a regular file)?"
  (when-let* (((derived-mode-p 'dired-mode))
              (entry (dired-get-filename nil t)))
    (file-directory-p entry)))

;;;###autoload
(defun akn/dirvish-inside-subtree-p ()
  "Is the cursor inside an indented subtree?"
  (and (featurep 'dirvish-subtree)
       (dirvish-subtree--parent)))

;;;###autoload
(defun akn/dirvish-subtree-expanded-p ()
  "If there's a directory at point, is it an expanded subtree?"
  (and (featurep 'dirvish-subtree)
       (dirvish-subtree--expanded-p)))

;;;###autoload
(defun akn/dirvish-layout-p ()
  (and-let* (((derived-mode-p 'dired-mode))
             ((featurep 'dirvish))
             (curr (dirvish-curr)))
    (dv-curr-layout curr)))

;;;###autoload
(defun akn/dirvish-do-layout ()
  "Run `dirvish-layout-toggle' if we're not already in a dirvish layout."
  (when (and (derived-mode-p 'dired-mode) (featurep 'dirvish))
    (unless (akn/dirvish-layout-p)
      (dirvish-layout-toggle))))

;;; Commands

;;;###autoload
(defun akn/dirvish-close-subtree-or-up-directory ()
  "Collapse the current subtree, or go up a directory."
  (interactive nil dired-mode)
  (if (akn/dirvish-subtree-expanded-p)
      (dirvish-subtree-toggle)
    (akn/dirvish-up-directory)))

;;;###autoload
(defun akn/dirvish-up-directory ()
  "Go up a directory (or, if in a subtree, go to the top of the subtree)."
  (interactive nil dired-mode)
  (if (akn/dirvish-inside-subtree-p)
      (dirvish-subtree-up)
    (dired-up-directory)))

;;;###autoload
(defun akn/dirvish-toggle-subtree-or-open-file ()
  "If on a directory, toggle the subtree. Otherwise, find the file.

Non-mouse version of `dirvish-subtree-toggle-or-open'."
  (interactive nil dired-mode)
  (if (akn/dired-directory-p)
      (dirvish-subtree-toggle)
    (dired-find-file)))

;;;###autoload
(defun akn/dired-goto-beginning ()
  (interactive nil dired-mode)
  (goto-char (point-min))
  (redisplay)
  (dlet ((dired-movement-style 'cycle-files))
    (goto-char (point-max))
    (dired-next-line 1)))

;;;###autoload
(defun akn/dired-goto-end ()
  (interactive nil dired-mode)
  (dlet ((dired-movement-style 'cycle-files))
    (goto-char (point-min))
    (dired-previous-line 1)))

;;;###autoload
(defun akn/dired-hide-subdir ()
  (interactive nil dired-mode)
  (save-excursion
    (call-interactively (if (save-excursion
                              (and (dired-file-name-at-point)
                                   (directory-name-p (dired-file-name-at-point))))
                            #'dired-maybe-insert-subdir
                          #'dired-hide-subdir))))

;; ;;;###autoload
;; (defun akn/dired-insert-or-hide-subdir ()
;;   "TODO"
;;   (interactive nil dired-mode))

;;;###autoload
(defalias 'akn/dirvish-transient #'dirvish-dispatch)

;;; manually dragging files up/down in dired
;; https://xenodium.com/interactive-ordering-of-dired-items

(defun ar/dired-drag-item-up ()
  "Drag dired item down in buffer."
  (interactive nil dired-mode)
  (unless (dired-get-filename nil t)
    (error "Not a dired draggable item"))
  (when (= (line-number-at-pos) 2)
    (error "Already at top"))
  (let* ((inhibit-read-only t)
         (col (current-column))
         (item-start (line-beginning-position))
         (item-end (1+ (line-end-position)))
         (item (buffer-substring item-start item-end)))
    (delete-region item-start item-end)
    (forward-line -1)
    (beginning-of-line)
    (insert item)
    (forward-line -1)
    (move-to-column col)))

(defun ar/dired-drag-item-down ()
  "Drag dired item down in buffer."
  (interactive nil dired-mode)
  (unless (dired-get-filename nil t)
    (error "Not a dired draggable item"))
  (when (save-excursion
          (forward-line 1)
          (eobp))
    (error "Already at bottom"))
  (let* ((inhibit-read-only t)
         (col (current-column))
         (item-start (line-beginning-position))
         (item-end (1+ (line-end-position)))
         (item (buffer-substring item-start item-end)))
    (delete-region item-start item-end)
    (forward-line 1)
    (beginning-of-line)
    (insert item)
    (forward-line -1)
    (move-to-column col)))
