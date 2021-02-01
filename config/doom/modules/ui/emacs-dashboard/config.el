;;; ui/emacs-dashboard/config.el -*- lexical-binding: t; -*-

(setq! dashboard-projects-backend (if (fboundp 'projectile-mode) 'projectile 'project-el)
       dashboard-center-content t
       dashboard-vertically-center-content t
       dashboard-items
       '((projects . 10)
         (agenda . 10)
         (bookmarks . 10)
         (recents . 10))
       dashboard-startupify-list
       '(;; dashboard-insert-banner
         ;; dashboard-insert-newline
         ;; dashboard-insert-banner-title
         ;; dashboard-insert-newline
         dashboard-insert-init-info
         dashboard-insert-items)
         ;; dashboard-insert-newline
         ;; dashboard-insert-footer)
       dashboard-navigation-cycle t
       dashboard-display-icons-p t
       ;dashboard-startup-banner 'ascii
       dashboard-icon-type 'nerd-icons)

;; (add-hook 'dashboard-after-initialize-hook #'dashboard-next-section)

;;; Bootstrap

(defun +emacs-dashboard-init-h ()
  "Initializes Doom's dashboard."
  (unless noninteractive
    (require 'dashboard)
    ;; Ensure the dashboard becomes Emacs' go-to buffer when there's nothing
    ;; else to show.
    (setq doom-fallback-buffer-name dashboard-buffer-name
          initial-buffer-choice #'doom-fallback-buffer)
    (when (equal (buffer-name) "*scratch*")
      (let ((switch-to-buffer-obey-display-actions nil)
            (switch-to-buffer-preserve-window-point t))
        (dashboard-open)))
    (add-hook 'doom-load-theme-hook #'+emacs-dashboard-reload-force)
    ;; Ensure the dashboard is up-to-date whenever it is switched to or resized.
    (add-hook 'window-configuration-change-hook #'dashboard-resize-on-hook 100)
    (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)
    (add-hook 'doom-switch-buffer-hook #'+emacs-dashboard-reload-maybe)
    (add-hook 'delete-frame-functions #'+emacs-dashboard-reload-frame-h)
    ;; `persp-mode' integration: update `default-directory' when switching perspectives
    ;; (add-hook 'persp-created-functions #'+doom-dashboard--persp-record-project-h)
    ;; (add-hook 'persp-activated-functions #'+doom-dashboard--persp-detect-project-h)
    ;; HACK Fix #2219 where, in GUI daemon frames, the dashboard loses center
    ;;      alignment after switching (or killing) workspaces.
    (when (daemonp)
      (add-hook 'persp-activated-functions #'+emacs-dashboard-reload-maybe))))
    ;; (add-hook 'persp-before-switch-functions #'+doom-dashboard--persp-record-project-h)))

(add-hook 'doom-init-ui-hook #'+emacs-dashboard-init-h 'append)

(defun +emacs-dashboard-reload-force ()
  (require 'dashboard)
  (dashboard-insert-startupify-lists t))
(defun +emacs-dashboard-reload-maybe ()
  (require 'dashboard)
  (dashboard-insert-startupify-lists))

(defvar +emacs-dashboard--reload-timer nil)
(defun +emacs-dashboard-reload-frame-h (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (when (timerp +emacs-dashboard--reload-timer)
    (cancel-timer +emacs-dashboard--reload-timer)) ; in case this function is run rapidly
  (setq +emacs-dashboard--reload-timer
        (run-with-timer 0.1 nil #'+emacs-dashboard-reload-force t)))
