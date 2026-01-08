;;; config/akn-bindings/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun akn/last-buffer-other-window ()
  (interactive)
  (save-selected-window
    (call-interactively #'other-window)
    (call-interactively
     (if (modulep! :editor evil)
         #'evil-switch-to-windows-last-buffer
       (kmacro "C-x b RET")))))

;;;###autoload
(defun akn/open-new-emacs ()
  (interactive)
  (start-process "emacs-open-new" nil "emacs-open-new"))

;;;###autoload
(defun akn/open-new-emacs-with-profile (profile)
  (interactive
   (list (completing-read "Profile: "
                          (progn
                            (require 'doom-profiles)
                            (doom-profiles-autodetect)))))
  (start-process-shell-command "emacs-open-new" nil (format "emacs-open-new --profile %s" profile)))

;;;###autoload
(defun akn/open-in-vscode (&optional path)
  (interactive)
  (akn/run-command (format "code '%s'"
                           (or path
                               buffer-file-name
                               default-directory
                               (doom-project-root)))))
;;;###autoload
(defun akn/open-project-in-vscode (&optional path)
  (interactive)
  (akn/open-in-vscode (or (and path (file-directory-p path) path)
                         (and path (file-regular-p path) (file-name-directory path))
                         (doom-project-root)
                         default-directory)))

;;;###autoload
(defun akn/doom-reload-restart ()
  (interactive)
  (add-hook! 'doom-after-reload-hook #'akn/restart)
  (doom/reload))

;;;###autoload
(defun akn/find-sibling-file-other-window (&rest args)
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (save-selected-window  ; not sure whether I want to save selected window
    (save-window-excursion
      (apply #'find-sibling-file args))
    (let ((display-buffer-overriding-action (cons nil '((inhibit-same-window . t)))))
      (apply #'find-sibling-file args))))

;;;###autoload
(defun akn/toggle-all-ligatures ()
  (interactive)
  (let ((enable-or-disable
         (if (or (bound-and-true-p prettify-symbols-mode)
                 (bound-and-true-p auto-composition-mode))
             -1
           1)))
    (prettify-symbols-mode enable-or-disable)
    (auto-composition-mode enable-or-disable)))

;;;###autoload
(defun akn/duplicate-up ()
  (interactive)
  (duplicate-line))
;;;###autoload
(defun akn/duplicate-down ()
  (interactive)
  (duplicate-line)
  (call-interactively #'next-line))

;;;###autoload
(defun akn/yank-no-transform (&optional arg)
  (interactive "*P")
  (setq this-command #'yank)
  (let (yank-transform-functions)
    (yank arg)))

;; TODO: make this work with regular :ui workspaces too
;; TODO: make this work with no workspaces module enabled
;;;###autoload
(defun akn/switch-project (project)
  (interactive
   (progn
     (require 'projectile)
     (list
      (let ((projects (projectile-relevant-known-projects)))
        (if projects
            (projectile-completing-read "Switch to project: " projects)
          (user-error "There are no known projects"))))))
  ;; +workspaces-switch-project-function
  ;; projectile-switch-project-action
  ;; (run-hook-with-args 'akn/before-switch-project-hook project)
  (if (fboundp '+workspaces-switch-to-project-h)
      (+workspaces-switch-to-project-h project)
    (require 'projectile)
    (projectile-switch-project-by-name project)))
  ;; (projectile-switch-project-by-name project)
  ;; (projectile-find-file))
