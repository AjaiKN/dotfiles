;;; ui/tab-bar/autoload/tabspaces-sessions.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui tab-bar -bufferlo)

(require 'akn)
(require 'seq)

(defvar tabspaces-session-file)

;;;###autoload
(defun +tab-bar:doom-session-file ()
  (file-name-concat tabspaces-session-project-session-store
                    (concat
                     (pcase (daemonp)
                       ((and (pred stringp) s) s)
                       ('nil "session")
                       (_ "server"))
                     ".el")))

(defun +tab-bar--unique-tab-names ()
  (cl-loop for i below (length (+workspace-list))
           for name = (+workspace--name i)
           do (when (member name names)
                (let ((num 2))
                  (while (member (concat name "<" (number-to-string num) ">") names)
                    (cl-incf num))
                  (setq name (concat name "<" (number-to-string num) ">"))))
           collect name into names
           finally return names))

;;;###autoload
(defun +tab-bar:doom-save-session (&optional file)
  (let ((tabspaces-session-file (or file (+tab-bar:doom-session-file))))
    (when (seq-some #'doom-real-buffer-p (buffer-list))
      ;; TODO: temporarily uniquify tab names (or maybe just fix loading?)
      (shut-up
        (tabspaces-save-session))))
  t)

;;;###autoload
(defun +tab-bar:doom-load-session (&optional file)
  (let ((tabspaces-session-file (or file (+tab-bar:doom-session-file))))
    (when (null file)
      (+workspace/kill-session)
      (tab-bar-rename-tab "--deleteme--"))
    (tabspaces-restore-session file)
    (when-let* ((old (+workspace--index "--deleteme--")))
      (tab-bar-close-tab old))))

;;; from :ui workspaces

;; ;;;###autoload
;; (defun +workspace-load (name)
;;   "Loads a single workspace (named NAME) into the current session. Can only
;; retrieve tabs that were explicitly saved with `+workspace-save'.

;; Returns t if successful, nil otherwise."
;;   ())

;; ;;;###autoload
;; (defun +workspace/load (name)
;;   "Load a workspace and switch to it. If called with C-u, try to reload the
;; current workspace (by name) from session files."
;;   (interactive))

;; ;;;###autoload
;; (defun +workspace-save (name)
;;   "Saves a single workspace (NAME) from the current session. Can be loaded again
;; with `+workspace-load'. NAME can be the string name of a workspace or its
;; tabs hash table.

;; Returns t on success, nil otherwise."
;;   ())

;; ;;;###autoload
;; (defun +workspace/save (name)
;;   "Save the current workspace. If called with C-u, autosave the current
;; workspace."
;;   (interactive))

;;;###autoload
(defun +workspace/kill-session ()
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive)
  (dotimes (_ (length (+workspace-list)))
    (tab-bar-close-tab)))

;; ;;;###autoload
;; (defun +workspace/delete ()
;;   "Delete a saved workspace.

;; Can delete workspaces saved with `+workspace/save' or `+workspace-save'."
;;   (interactive))

;; ;;;###autoload
;; (defun +workspace/restore-last-session (&optional force)
;;   ;; NOTE: in :ui workspace, this is an alias for `doom/quickload-session'
;;   (interactive))
