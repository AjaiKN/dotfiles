;;; tools/prodigy/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +prodigy/create ()
  "Interactively create a new prodigy service."
  (interactive))
  ;; TODO


;;;###autoload
(defun +prodigy/delete (arg)
  "Delete service at point. Asks for confirmation."
  (interactive "P" prodigy-mode)
  (prodigy-with-refresh
   (when-let* ((service (prodigy-service-at-pos)))
     (let ((name (plist-get service :name)))
       (cond ((or arg
                  (y-or-n-p (format "Delete '%s' service?" name)))
              (setq prodigy-services (delete service prodigy-services))
              (ignore-errors
                (prodigy-goto-next-line))
              (message "Successfully deleted service: %s" name))
             (t
              (message "Aborted")))))))

;;;###autoload
(defun +prodigy/cleanup ()
  "Delete all services associated with projects that don't exist."
  (interactive nil prodigy-mode)
  (cl-loop for service in prodigy-services
           if (and (plist-member service :project)
                   (file-directory-p (plist-get service :project)))
           collect service into services
           finally do (setq prodigy-service services)))

;;;###autodef
(defun def-service-tag! (&rest args)
  (after! prodigy
    (apply #'prodigy-define-tag args)))

;;;###autodef
(defun defservice! (&rest args)
  "Wrapper for `prodigy-define-service' (see `prodigy-services')"
  (after! prodigy
    (let ((cwd (plist-get args :cwd)))
      (when-let* ((project (plist-get args :project)))
        (plist-put! args :cwd (if cwd
                                  (expand-file-name cwd project)
                                project))))
    (when (not (plist-get args :name))
      (if-let* ((command-or-tags (or (plist-get args :command)
                                     (when-let* ((tags (plist-get args :tags)))
                                       (string-join tags ", "))))
                (cwd (plist-get args :cwd))
                (name (format "%s (%s)" command-or-tags cwd)))
          (plist-put! args :name name)
        (warn "service %s has no name" args)))
    (when (not (plist-get args :args))
      (let ((command (plist-get args :command)))
        (cond
         ((stringp command)
          (plist-put! args
                      :command (car (split-string-shell-command command))
                      :args    (cdr (split-string-shell-command command))))
         ((functionp command)
          (plist-put! args
                      :command (lambda (&rest args) (car (split-string-shell-command (apply command args))))
                      :args    (lambda (&rest args) (cdr (split-string-shell-command (apply command args)))))))))

    ;; b/c Doom downcases the full directory name when checking project equivalence
    (when-let* ((project (akn/expand-file-downcase (plist-get args :project))))
      (plist-put! args :project project))
    (apply #'prodigy-define-service args)))
