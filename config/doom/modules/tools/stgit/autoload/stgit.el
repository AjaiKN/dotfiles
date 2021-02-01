;;; tools/stgit/autoload/stgit.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +stgit/status (&optional dir)
  (interactive)
  (stgit (or dir
             (and (require 'magit-process nil t)
                  (require 'magit-git nil t)
                  (magit-toplevel))
             default-directory)))
