;;; ui/tab-bar/autoload/bufferlo-sessions.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui tab-bar +bufferlo)

(require 'seq)

;;;###autoload
(defun +tab-bar:doom-save-session () ;(&optional file)
  (bookmark-maybe-load-default-file)
  (cl-letf (((frame-parameter nil 'bufferlo-bookmark-frame-name)
             (frame-parameter nil 'bufferlo-bookmark-frame-name)))
    (when (seq-some #'doom-real-buffer-p (buffer-list))
      (bufferlo-bookmark-frame-save (+tab-bar-frame-session-bookmark) nil t)))
  t)

;;;###autoload
(defun +tab-bar:doom-load-session () ;(&optional file)
  (bookmark-maybe-load-default-file)
  (bufferlo-bookmark-frame-load (+tab-bar-frame-session-bookmark))
  (setf (frame-parameter nil 'bufferlo-bookmark-frame-name) nil))


;;; from :ui workspaces

;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve tabs that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (bufferlo-bookmark-tab-load name)
  t)

;;;###autoload
(defun +workspace/load (name)
  "Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read
       "Load bufferlo tab bookmark: "
       (bufferlo--bookmark-get-names #'bufferlo--bookmark-tab-handler)
       nil 'require-match nil 'bufferlo-bookmark-tab-history
       (alist-get 'bufferlo-bookmark-tab-name (bufferlo--current-tab))))))
  (+workspace-load name))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
tabs hash table.

Returns t on success, nil otherwise."
  (bufferlo-bookmark-tab-save name)
  t)

;;;###autoload
(defun +workspace/save (name)
  "Save the current workspace. If called with C-u, autosave the current
workspace."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (list (completing-read
             "Save bufferlo tab bookmark: "
             (bufferlo--bookmark-get-names #'bufferlo--bookmark-tab-handler)
             nil nil nil 'bufferlo-bookmark-tab-history
             (alist-get 'bufferlo-bookmark-tab-name (bufferlo--current-tab)))))))
  (+workspace-save name))

;;;###autoload
(defun +workspace/kill-session ()
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive)
  (dotimes (_ (length (+workspace-list)))
    (bufferlo-tab-close-kill-buffers)))

;;;###autoload
(defun +workspace/delete (workspace)
  "Delete a saved workspace.

Can delete workspaces saved with `+workspace/save' or `+workspace-save'."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read
       "Load bufferlo tab bookmark: "
       (bufferlo--bookmark-get-names #'bufferlo--bookmark-tab-handler)
       nil 'require-match nil 'bufferlo-bookmark-tab-history
       (alist-get 'bufferlo-bookmark-tab-name (bufferlo--current-tab))))))
  (bookmark-delete workspace))

;;;###autoload
(defun +workspace/restore-last-session (&optional force)
  ;; NOTE: in :ui workspace, this is an alias for `doom/quickload-session'
  (interactive)
  (if (or force
          (not (seq-some #'doom-real-buffer-p (buffer-list)))
          (yes-or-no-p "This will wipe your current session, do you want to continue? "))
      (progn (message "Restoring session...")
             (+tab-bar:doom-load-session)
             (message "Restoring session...done"))
    (message "Session not restored.")))
