;;; tools/fasd/config.el -*- lexical-binding: t; -*-

;; inspired by https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Btools/fasd/packages.el

(defun +fasd/find-directory-only ()
  (interactive)
  (fasd-find-file 2))
(defun +fasd/find-file-only ()
  (interactive)
  (fasd-find-file -1))

(use-package! fasd
  :defer t
  :defer-incrementally t
  :after-call doom-first-file-hook
  :config
  (setq! fasd-enable-initial-prompt nil)
  (global-fasd-mode)
  (after! marginalia
    (add-to-list 'marginalia-prompt-categories '("\\<fasd\\>" . file)))

  ;; TODO: maybe PR: Since this is only run on find-file-hook, if you open a new
  ;; file, it'll see that it's not saved, so it won't add it to fasd.
  (define-advice fasd-add-file-to-db (:override () akn/fix-new-file-a)
    "Add current file or directory to the Fasd database."
    (if (not (executable-find "fasd"))
        (message "Fasd executable cannot be found. It is required by `fasd.el'. Cannot add file/directory to the fasd db")
      (let ((file (if (string= major-mode "dired-mode")
                      dired-directory
                    (buffer-file-name))))
        (when (and file
                   (stringp file))
          (if (file-readable-p file)
              (progn (start-process "*fasd*" nil "fasd" "--add" file)
                     (remove-hook 'after-save-hook #'fasd-add-file-to-db t))
            ;; ADDED
            (add-hook 'after-save-hook #'fasd-add-file-to-db nil t)))))))

;; from https://github.com/karthink/consult-dir#writing-your-own-directory-source
(after! (:and fasd consult-dir)
  ;; A function that returns a list of directories
  (defun consult-dir--fasd-dirs ()
    "Return list of fasd dirs."
    (split-string (shell-command-to-string "fasd -ld") "\n" t))

  ;; A consult source that calls this function
  (defvar consult-dir--source-fasd
    `(:name     "Fasd dirs"
      :narrow   ?f
      :category file
      :face     consult-file
      :history  file-name-history
      :enabled  ,(lambda () (executable-find "fasd"))
      :items    ,#'consult-dir--fasd-dirs)
    "Fasd directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-fasd t))
