;;; os/exwm/start.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'akn)

;; Set the initial workspace number.
;; (setq exwm-workspace-number 4)
;; Make class name the buffer name.
(add-hook! 'exwm-update-class-hook
  (defun +exwm--set-buffer-name-to-class-name-h ()
    (exwm-workspace-rename-buffer exwm-class-name)))
;; Global keybindings.
(setq! exwm-input-global-keys
       `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
         ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
         ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command cmd nil cmd)))
         ;; s-N: Switch to certain workspace.
         ,@(mapcar (lambda (i)
                     `(,(kbd (format "s-%d" i)) .
                       (lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
                   (number-sequence 0 9))))
(setq! exwm-input-prefix-keys
       `(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:
         ?\M-\s
         ?\s-h ?\s-j ?\s-k ?\s-l
         ?\s-\M-h ?\s-\M-j ?\s-\M-k ?\s-\M-l))
(map! :map exwm-mode-map
      "M-SPC" nil)
;; Enable EXWM
(exwm-wm-mode)

(defun +exwm/fix-screen-size ()
  (interactive)
  (doom-log "fixing screen size")
  (shut-up
    (make-frame))
  (akn/after-timer! (0)
    (delete-frame)
    (doom-log "done fixing screen size")))

(add-hook! 'doom-init-ui-hook :depth 200
  (doom-log "init-ui-hook running")
  (akn/after-timer! (1)
    (+exwm/fix-screen-size)))
