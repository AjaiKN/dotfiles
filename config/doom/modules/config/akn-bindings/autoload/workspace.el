;;; -*- lexical-binding: t; -*-
;;;###if (or (modulep! :ui workspaces) (modulep! :ui tabs))

;;;###autoload
(defun akn/move-buffer-to-workspace (workspace-name &optional buffer dont-switch-to-workspace-p)
  "Move BUFFER (by default, the current buffer) to the specified workspace.
Interactively, prompts to ask for the WORKSPACE-NAME.
If there's no workspace by that name, it will be created.
Unless DONT-SWITCH-TO-WORKSPACE-P (or interactively, a prefix argument)
is provided, switch to that workspace."
  (interactive (list (let* ((names (+workspace-list-names))
                            (alist (cl-loop for name in names
                                            for i from 1 to (length names)
                                            collect (cons (format "[%d] %s" i name) name)))
                            (res (completing-read "Workspace: " alist)))
                       (message "%s %s %s" res alist (alist-get res alist res))
                       (alist-get res alist res nil #'equal))
                     nil
                     current-prefix-arg))
  (when (null buffer)
    (setq buffer (current-buffer)))
  (unless (+workspace-exists-p workspace-name)
    (+workspace-new workspace-name))
  (let ((orig-workspace (+workspace-current))
        (orig-workspace-name (+workspace-current-name))
        (new-workspace (+workspace-get workspace-name)))
    (+workspace-switch workspace-name)
    ;; (persp-add-buffer buffer new-workspace t)
    (pop-to-buffer-same-window buffer)
    (+workspace-switch orig-workspace-name)
    (when (+popup-window-p)
      (+popup/close))
    (cond
     ((fboundp #'persp-remove-buffer)
      (persp-remove-buffer buffer orig-workspace nil t))
     ((fboundp #'tabspaces-remove-buffer)
      (tabspaces-remove-buffer buffer)
      (when (eq (current-buffer) buffer)
        (switch-to-buffer doom-fallback-buffer-name)
        (tabspaces-remove-buffer buffer)))
     ((fboundp #'bufferlo-remove)
      (bufferlo-remove buffer)
      (when (eq (current-buffer) buffer)
        (switch-to-buffer doom-fallback-buffer-name)
        (bufferlo-remove buffer))))
    (unless dont-switch-to-workspace-p
      (+workspace-switch workspace-name)))
  (+workspace/display))

;;;###autoload
(defun akn/other-workspace-prefix (workspace-name)
  "Display the buffer of the next command in the specified workspace.
Interactively, prompts to ask for the WORKSPACE-NAME.
If there's no workspace by that name, it will be created.

The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive (list (completing-read "Workspace: " (+workspace-list-names))))
  (display-buffer-override-next-command
   (lambda (_buffer _alist)
     (let ((workspace-existed-before-p (+workspace-exists-p workspace-name))
           (display-buffer-overriding-action (cons nil nil))
           window
           type)
       (unless workspace-existed-before-p
         (+workspace-new workspace-name))
       (+workspace-switch workspace-name)
       (setq window (selected-window)
             type (if workspace-existed-before-p 'reuse 'window))
       (cons window type)))
   nil (format "[%s%s]" workspace-name (if (+workspace-exists-p workspace-name) "*" "")))
  (message "Display next command buffer in workspace %s..." workspace-name))

;;;###autoload
(defun akn/new-workspace-prefix (&optional workspace-name)
  "Display the buffer of the next command in a new workspace.
Interactively, if a prefix argument is provided, prompt to ask
for the WORKSPACE-NAME. If a workspace by that name already
exists, use that existing workspace.

The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive (list (when current-prefix-arg (completing-read "Workspace: " (+workspace-list-names)))))
  (setq workspace-name (or workspace-name (+workspace--generate-id)))
  (akn/other-workspace-prefix workspace-name))
