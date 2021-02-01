;;; tools/prodigy/config.el -*- lexical-binding: t; -*-

(require 'akn)

(use-package! prodigy
  :defer t
  :defer-incrementally (s dash f ansi-color tabulated-list easymenu hl-line)
  :commands (+prodigy/prodigy)
  :config
  (defadvice! +prodigy--add-project-property-a (fn &rest args)
    "Adds a new :project property to prodigy services, which hides the service
unless invoked from the relevant project."
    :around #'prodigy-services
    (let ((project-root (downcase (or (doom-project-root) default-directory)))
          (services (apply fn args)))
      (if current-prefix-arg
          services
        (cl-remove-if-not (lambda (service)
                            (let ((project (plist-get service :project)))
                              (or (not project)
                                  (file-in-directory-p project-root project))))
                          services))))

  (map! :map prodigy-mode-map
        "d" #'+prodigy/delete
        :nvieomrg "r" #'prodigy-restart)

  (defun +prodigy-buffer-name ()
    (format "*prodigy* (%s)" (+workspace-current-name)))
  (defadvice! +prodigy--set-buffer-name-a (&rest _)
    :before #'prodigy-buffer
    :before #'prodigy
    (setq-local prodigy-buffer-name (+prodigy-buffer-name)))
  (defun +prodigy/prodigy ()
    (interactive)
    (let ((dir default-directory)
          (root (doom-project-root))
          (prodigy-buffer-name (+prodigy-buffer-name)))
      (prodigy)
      (setq-local default-directory dir
                  projectile-project-root root
                  prodigy-buffer-name prodigy-buffer-name)
      (revert-buffer)
      (set-window-dedicated-p (selected-window) t)))
  (defun akn/prodigy-reset ()
    (interactive)
    (setq prodigy-services nil)
    (setq prodigy-tags nil))

  (defun akn/port-in-use-p (port)
    (and port
         (eql (car (doom-call-process "lsof" "-i" (format ":%s" port)))
              0)))
  ;; ^ TODO: use advice around `prodigy-service-port' to warn if port already in use

  (set-popup-rule! (rx "*prodigy*"))
  (set-popup-rule! (rx "*prodigy-") :width 0.35 :actions (list #'display-buffer-pop-up-window
                                                               #'display-buffer-in-previous-window
                                                               #'display-buffer-use-least-recent-window))

  (defadvice! akn/prodigy-service-stop-signal-default-a (&rest _)
    :after-until #'prodigy-service-stop-signal
    'int))
