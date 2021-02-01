;;; ui/tab-bar/autoload/workspace.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'tab-bar)
(eval-when-compile
  (require 'macroexp))

(if (modulep! +bufferlo)
    (require 'bufferlo)
  (require 'tabspaces))

;;; Library

(defun +workspace--protected-p (name)
  (and (modulep! -bufferlo)
       (equal name tabspaces-default-tab)))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))

;;;###autoload
(defmacro +workspaces--save-tab-excursion (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (tab-before)
    `(let ((,tab-before (+workspace--current-index)))
       (unwind-protect ,(macroexp-progn body)
         (+workspace/switch-to ,tab-before)))))

;;;###autoload
(defmacro +workspaces--with-selected-tab (tab-num &rest body)
  (declare (indent 1))
  `(+workspaces--save-tab-excursion
     (tab-bar-select-tab ,tab-num)
     ,@body))

(defmacro +workspaces--with-permanent-tab-names (&rest body)
  (declare (indent 0))
  (cl-with-gensyms (temporary-name-tabs)
    `(let ((,temporary-name-tabs
            (cl-loop for tab in (tab-bar-tabs)
                     for i from 0
                     when (not (alist-get 'explicit-name tab))
                     collect (progn (tab-bar-rename-tab (alist-get 'name tab) i)
                                    i))))
       (unwind-protect ,(macroexp-progn body)
         (dolist (i ,temporary-name-tabs)
           (tab-bar-rename-tab "" i))))))

;;; Predicates

(defun +workspace-p (obj)
  "Returns non-nil if OBJ is a tab object, the name of a tab, or a tab
index."
  (+workspace--get obj))

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (+workspace--get name))

;;;###autoload
(defun +workspace-contains-buffer-p (&optional buffer workspace)
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace)."
  (memq (or buffer (current-buffer))
        (+workspace-buffer-list workspace)))

(defun +workspace--current-p (workspace)
  (eq (car-safe (+workspace--get workspace))
      'current-tab))

;;; Getters

;; see `tab-bar--current-tab-find' and friends

(defun +workspace--tab-bar-tabs-with-explicit-names ()
  (seq-map (lambda (tab) (when (alist-get 'explicit-name tab) tab))
           (tab-bar-tabs)))

(defun +workspace--get (thing)
  (cond
   ((stringp thing)
    (or (seq-find (lambda (workspace) (equal (alist-get 'name workspace) thing))
                  (+workspace--tab-bar-tabs-with-explicit-names))
        (seq-find (lambda (workspace) (equal (alist-get 'name workspace) thing))
                  (tab-bar-tabs))))
   ((numberp thing)
    (nth thing (tab-bar-tabs)))
   ((listp thing)
    (when (memq thing (tab-bar-tabs))
      thing))))
(defun +workspace--index (thing)
  (cond
   ((stringp thing)
    (or (cl-position-if (lambda (workspace) (equal (alist-get 'name workspace) thing))
                        (+workspace--tab-bar-tabs-with-explicit-names))
        (cl-position-if (lambda (workspace) (equal (alist-get 'name workspace) thing))
                        (tab-bar-tabs))))
   ((numberp thing)
    (when (nth thing (tab-bar-tabs))
      thing))
   ((listp thing)
    (seq-position (tab-bar-tabs) thing #'eq))))
(defun +workspace--name (thing)
  (cond
   ((listp thing)
    (alist-get 'name thing))
   ((stringp thing)
    (when (seq-some (lambda (workspace) (equal (alist-get 'name workspace) thing))
                    (tab-bar-tabs))
      thing))
   ((numberp thing)
    (alist-get 'name (nth thing (tab-bar-tabs))))))

;;;###autoload
(defun +workspace-current ()
  "Return the currently active workspace."
  (seq-find (lambda (tab) (eq (car-safe tab) 'current-tab))
            (tab-bar-tabs)))

(defun +workspace--current-index ()
  (cl-position-if (lambda (tab) (eq (car-safe tab) 'current-tab))
                  (tab-bar-tabs)))
(defun +workspace--current-tab-number ()
  (1+ (+workspace--current-index)))

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (when-let* ((workspace (+workspace--get name)))
    (if (or workspace noerror)
        workspace
      (error "No workspace called '%s' was found" name))))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (+workspace--name (+workspace-current)))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  (mapcar #'+workspace--name (tab-bar-tabs)))

;;;###autoload
(defun +workspace-list ()
  "Return a list of workspace objects (satisifes `+workspace-p')."
  (tab-bar-tabs))

;;;###autoload
(defun +workspace-buffer-list (&optional workspace)
  "Return a list of buffers in WORKSPACE.

WORKSPACE can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((ind (and workspace
                  ;; `+workspace--buffer-list' defaults to the current
                  ;; workspace, and it's faster if its argument is nil.
                  (not (+workspace--current-p workspace))
                  (+workspace--index workspace))))
    (cl-loop for buffer in (+workspace--buffer-list nil ind)
             when (and (doom-real-buffer-p buffer)
                       (+workspace--local-buffer-p buffer))
             collect buffer)))

;;;###autoload
(defun +workspace-empty-p (&optional workspace)
  "Is WORKSPACE empty?

WORKSPACE can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (null (+workspace-buffer-list workspace)))

;;; Actions

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return the new workspace on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (+workspace--get
   (+workspaces--save-tab-excursion
     (+workspace/new name)
     (+workspace--current-index))))

;;;###autoload
(defalias '+workspace-rename #'tab-bar-rename-tab-by-name
  "Rename the current workspace named TAB-NAME to NEW-NAME. Returns old name on
success, nil otherwise.")

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (let ((workspace-ind (or (+workspace--index name)
                           (if (and auto-create-p (stringp name))
                               (+workspace--index (+workspace-new name))
                             (error "%S is not an available workspace" name))))
        (old-ind (+workspace--current-index)))
    (unless (equal old-ind workspace-ind)
      (+workspace/switch-to workspace-ind))
    (+workspace-current)))

;;; bufferlo/tabspaces Aliases

;;;###autoload (autoload '+workspace/switch-to-buffer "config/doom/modules/ui/tab-bar/autoload/workspace" nil t)
;;;###autoload (autoload '+workspace/switch-buffer-and-tab "config/doom/modules/ui/tab-bar/autoload/workspace" nil t)
;;;###autoload (autoload '+workspace/kill-buffers-close-workspace "config/doom/modules/ui/tab-bar/autoload/workspace" nil t)
;;;###autoload (autoload '+workspace-remove-buffer "config/doom/modules/ui/tab-bar/autoload/workspace" nil t)

(if (modulep! :ui tab-bar +bufferlo)
    (progn
      (defalias '+workspace/switch-to-buffer             #'bufferlo-switch-to-buffer)
      (defalias '+workspace/switch-buffer-and-tab        #'bufferlo-switch-buffer-and-tab)
      (defalias '+workspace/kill-buffers-close-workspace #'bufferlo-tab-close-kill-buffers)
      (defalias '+workspace-remove-buffer                #'bufferlo-remove)
      (defalias '+workspace--buffer-list                 #'bufferlo-buffer-list)
      (defalias '+workspace--local-buffer-p              #'bufferlo-local-buffer-p))
  (defalias '+workspace/switch-to-buffer             #'tabspaces-switch-to-buffer)
  (defalias '+workspace/switch-buffer-and-tab        #'tabspaces-switch-buffer-and-tab)
  (defalias '+workspace/kill-buffers-close-workspace #'tabspaces-kill-buffers-close-workspace)
  (defalias '+workspace-remove-buffer                #'tabspaces-remove-buffer)
  (defalias '+workspace--buffer-list                 #'tabspaces--buffer-list)
  (defalias '+workspace--local-buffer-p              #'tabspaces--local-buffer-p))

;;; Commands

;;;###autoload
(defalias '+workspace/cycle #'tab-bar-switch-to-next-tab
  "Cycle ARG workspaces to the right (default) or left.")

;;;###autoload
(defun +workspace/switch-left (&optional n)  (interactive "p") (+workspace/cycle (- n)))
;;;###autoload
(defun +workspace/switch-right (&optional n) (interactive "p") (+workspace/cycle n))

;;;###autoload
(defalias '+workspace/kill #'tab-bar-close-tab
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete.")

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (let ((tab-bar-new-tab-choice (if clone-p 'clone tab-bar-new-tab-choice)))
    (tab-bar-new-tab)
    (when name
      (tab-bar-rename-tab name))
    (+workspace-current)))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (progn
    (setq index (or (+workspace--index index) index))
    (tab-bar-select-tab (if (>= index 0)
                            (1+ index)
                          index))
    (+workspace-current)))

;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "+workspace/switch-to-%d" i))
    (lambda () (interactive) (+workspace/switch-to i))
    (format "Switch to workspace #%d" (1+ i))))
;;;###autoload
(defun +workspace/switch-to-final () (interactive) (tab-bar-select-tab -1))

;;;###autoload
(defalias '+workspace/other #'tab-bar-switch-to-recent-tab
  "Switch to the last activated workspace.")

;;;###autoload
(defalias '+workspace/swap-left #'tab-bar-move-tab-backward)
;;;###autoload
(defalias '+workspace/swap-right #'tab-bar-move-tab)

;;;###autoload
(defalias '+workspace/rename #'tab-bar-rename-tab
  "Rename the current workspace.")

;;;###autoload
(defun +workspace/force-keep-current-name ()
  "Make the current tab's name permanent."
  (interactive)
  (tab-bar-rename-tab (funcall tab-bar-tab-name-function)))
;;;###autoload
(defun +workspace/force-auto-name ()
  "Use automatic (non-permanent) name for the current tab."
  (interactive)
  (with-current-buffer (doom-fallback-buffer)
    (setq default-directory "~"
          +doom-dashboard--last-cwd nil))
  (tab-bar-rename-tab ""))

;;;###autoload
(defun +workspace/new-named (name)
  "Create a new workspace with a given NAME."
  (interactive "sWorkspace Name: ")
  (+workspace/new name))

;;; Tabs display in minibuffer

;;; Hooks

(defvar activities-name-prefix "α: ")

(defun +workspaces--remove-activities-prefix (name)
  (if (and (stringp name)
           (string-match (rx bos
                             (literal activities-name-prefix)
                             (group (* anything)) eos)
                         name))
      (match-string 1 name)
    name))
(defun +workspaces--add-activities-prefix (name)
  (if (stringp name)
      (concat activities-name-prefix name)
    name))
(defun +workspaces--activity-p (&optional ws)
  (string-prefix-p activities-name-prefix
                   (+workspace--name (or ws (+workspace-current-name)))))

;; also see `tabspaces-open-or-create-project-and-workspace', which does something similar
;;;###autoload
(defun +workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project. If one already exists, switch
to it. If in the main workspace and it's empty, recycle that workspace, without
renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By default, this prompts
the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (let* ((dir (or dir default-directory))
         (proj-name (doom-project-name dir))
         (proj-root (file-truename dir))
         ;; HACK Clear projectile-project-root, otherwise cached roots may interfere
         ;;      with project switch (see #3166)
         projectile-project-root)
    (when (and tab-bar-mode dir)
      (unwind-protect
          (if (and (not (null +workspaces-on-switch-project-behavior))
                   (or (eq +workspaces-on-switch-project-behavior t)
                       (+workspace--protected-p (+workspace--name (+workspace-current)))
                       (+workspace-buffer-list)))
              (let* ((mapped-name (alist-get proj-root (bound-and-true-p tabspaces-project-tab-map) nil nil #'equal))
                     (existing-tab (or (and (equal (+workspaces--remove-activities-prefix (+workspace-current-name))
                                                   (or mapped-name proj-name))
                                            (+workspace-current))
                                       (and mapped-name (or (+workspace-get (+workspaces--add-activities-prefix mapped-name) 'noerror)
                                                            (+workspace-get mapped-name 'noerror)))
                                       (or (+workspace-get (+workspaces--add-activities-prefix proj-name) 'noerror)
                                           (+workspace-get proj-name 'noerror))))
                     (tab (or (and existing-tab (+workspace-switch existing-tab))
                              (+workspace/new proj-name))))
                (with-current-buffer (doom-fallback-buffer)
                  (setq-local default-directory proj-root)
                  (hack-dir-local-variables-non-file-buffer))
                (unless (+workspaces--activity-p)
                  (message "Renaming to %s" (+workspace--name tab))
                  (+workspace/force-keep-current-name))
                (when (boundp 'tabspaces-project-tab-map)
                  (setf (alist-get proj-root tabspaces-project-tab-map nil nil #'equal) (+workspace--name tab)))
                (unless current-prefix-arg
                  (funcall +workspaces-switch-project-function proj-root))
                (+workspace-message
                 (if existing-tab
                     (format "Switched to '%s'" proj-name)
                   (format "Switched to '%s' in new workspace" proj-name))
                 'success))
            ;; use current workspace
            (with-current-buffer (doom-fallback-buffer)
              (setq default-directory proj-root)
              (hack-dir-local-variables-non-file-buffer)
              (message "Switched to '%s' in current workspace" (doom-project-name proj-root)))
            (unless (+workspaces--activity-p)
              (with-demoted-errors "Workspace error: %s"
                (tab-bar-rename-tab (doom-project-name proj-root))))
            (when (boundp 'tabspaces-project-tab-map)
              (setf (alist-get proj-root tabspaces-project-tab-map nil nil #'equal) (+workspace-current-name)))
            (unless current-prefix-arg
              (funcall +workspaces-switch-project-function proj-root)))
        (unless (+workspaces--activity-p)
          (tab-bar-rename-tab (+workspace-current-name)))
        (run-hooks 'projectile-after-switch-project-hook)))))


;;; Advice

;;;###autoload
(defun +workspaces--with-permanent-tab-names-a (fn &rest args)
  (+workspaces--with-permanent-tab-names
    (apply fn args)))

;;; Tabs display in minibuffer

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-index (+workspace--current-index)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (= i current-index)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defun +workspace--message-body (message &optional type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

;;;###autoload
(defun +workspace-message (message &optional type)
  "Show an `elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an `elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))
