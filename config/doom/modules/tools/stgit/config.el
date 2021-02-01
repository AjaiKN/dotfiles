;;; tools/stgit/config.el -*- lexical-binding: t; -*-

(use-package! stgit
  :defer t
  :commands (stgit)
  :config
  (map! :map stgit-mode-map
        "s-?" #'+stgit/transient)

  (when (modulep! :editor evil)
    (evil-make-overriding-map stgit-mode-map))

  (add-to-list 'doom-real-buffer-functions #'+stgit-buffer-p)
  (defun +stgit-buffer-p (buffer)
    (let ((case-fold-search t))
      (string-match-p (rx bos "*stgit") (buffer-name buffer)))))
