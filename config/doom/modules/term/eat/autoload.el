;;; term/eat/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +eat-term-name-fn ()
  (getenv "TERM"))

;;;###autoload
(defun +eat-line-input-send-fn ()
  (eat-line-send-default)
  (evil-insert-state))

;;;###autoload
(defalias '+eat/toggle #'eat-project)

;;;###autoload
(defun +eat/here (&optional use-existing-p)
  (interactive "P")
  (let (display-buffer-alist)
    (eat-project (not use-existing-p))))
