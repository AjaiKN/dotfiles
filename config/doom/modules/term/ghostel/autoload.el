;;; term/ghostel/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+ghostel/toggle #'ghostel-project)

;;;###autoload
(defun +ghostel/here (&optional use-existing-p)
  (interactive "P")
  (let (display-buffer-alist)
    (+ghostel/toggle (not use-existing-p))))
