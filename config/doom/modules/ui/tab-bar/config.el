;;; ui/tab-bar/config.el -*- lexical-binding: t; -*-

(require 'akn)

(defvar +workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a project.

Can be one of the following:

t           If the project has an associated workspace, switch to it.
            Otherwise, create a new workspace.
non-empty   If the project has an associated workspace, switch to it.
            Otherwise, create a new workspace, or reuse the current one
            if it doesn't have any buffers.
nil         Never create or switch workspaces on project switch.")

(defvar +workspaces-switch-project-function #'doom-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one argument: the
new project directory.")

(defun +tab-bar-frame-session-bookmark ()
  (format "*frame-session-%s*"
          (pcase (daemonp)
            ((and (pred stringp) s) s)
            ('nil "session")
            (_ "server"))))

(add-hook 'after-init-hook #'tab-bar-mode)
(add-hook 'after-init-hook (if (modulep! +bufferlo)
                               #'bufferlo-mode
                             #'tabspaces-mode))

(add-hook! 'doom-init-ui-hook :depth 99
  (setq! tab-bar-new-tab-choice doom-fallback-buffer-name))

;;; tab-bar
(use-package! tab-bar
  :defer t
  :defer-incrementally t
  :commands +tab-bar-show-mode +tab-bar-show-in-minibuffer-mode
  :config
  (tab-bar-history-mode)
  (setq! tab-bar-tab-hints t
         tab-bar-new-tab-to 'rightmost
         tab-bar-auto-width-max (let ((n 50)) `((,(* n 11)) ,n))
         tab-bar-tab-name-function (lambda () (or (doom-project-name) "main"))
         tab-bar-show t
         tab-bar-history-limit 500
         tab-bar-close-last-tab-choice
         (lambda (_tab-to-close)
           (if (or t (not (display-graphic-p)))
               (doom/delete-frame-with-prompt)
             (+workspace/clear-tab))))

  (define-minor-mode +tab-bar-show-mode
    "Show the tab bar at the top of the screen."
    :global t
    :variable tab-bar-show
    :group 'akn
    (when (and tab-bar-show (not tab-bar-mode))
      (tab-bar-mode))
    ;; trigger the defcustom setter
    (setq! tab-bar-show tab-bar-show))
  (define-minor-mode +tab-bar-show-in-minibuffer-mode
    "Show the tab bar in the minibuffer when switching tabs."
    :global t
    :init-value t
    :group 'akn)

  (defadvice! +tab-bar--display-in-minibuffer-a (&rest _)
    :after #'+workspace/load
    :after #'tab-bar-switch-to-next-tab
    :after #'+workspace/new
    :after #'+workspace/switch-to
    :after #'tab-bar-move-tab
    :after #'tab-bar-move-tab-backward
    :after #'tab-bar-close-tab
    :after #'tab-bar-undo-close-tab
    (when +tab-bar-show-in-minibuffer-mode
      (+workspace/display))))

;;; projectile
(after! projectile
  ;; coppied from :ui workspaces
  ;; per-project workspaces, but reuse current workspace if empty
  ;; (setq projectile-switch-project-action #'+workspaces-switch-to-project-h)
  (setq projectile-switch-project-action
        (lambda ()
          (when (bound-and-true-p +workspaces-switch-project-function)
            (funcall +workspaces-switch-project-function default-directory)))
        counsel-projectile-switch-project-action
        '(1
          ("o" +workspaces-switch-to-project-h "open project in new workspace")
          ("O" counsel-projectile-switch-project-action "jump to a project buffer or file")
          ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
          ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
          ("D" counsel-projectile-switch-project-action-dired "open project in dired")
          ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
          ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
          ("w" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
          ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
          ("r" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
          ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
          ("C" counsel-projectile-switch-project-action-configure "run project configure command")
          ("e" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
          ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
          ("s" (lambda (project)
                 (let ((projectile-switch-project-action
                        (lambda () (call-interactively #'+ivy/project-search))))
                   (counsel-projectile-switch-project-by-name project))) "search project")
          ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
          ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
          ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
          ("X" counsel-projectile-switch-project-action-org-capture "org-capture into project"))))

;;; tabspaces

(use-package! tabspaces
  :when (modulep! -bufferlo)
  :defer t
  :defer-incrementally tab-bar project vc seq cl-lib dired-x
  :init
  ;;;; sessions
  (setq! tabspaces-session nil ; do this ourselves better
         tabspaces-session-auto-restore nil
         tabspaces-session-project-session-store (file-name-concat doom-state-dir "tabspaces-sessions/")
         tabspaces-session-file (+tab-bar:doom-session-file))
  (unless (file-exists-p tabspaces-session-project-session-store)
    (mkdir tabspaces-session-project-session-store))

  (setq tabspaces-keymap-prefix nil)
  :config
  (setq! tabspaces-default-tab "main"
         tabspaces-remove-to-default t
         tabspaces-exclude-buffers (list (regexp-opt (list doom-fallback-buffer-name "*scratch*")))
         tabspaces-include-buffers (list)
         tabspaces-initialize-project-with-todo nil
         tabspaces-keymap-prefix nil)

  (add-hook! 'tabspaces-mode-hook
    (defun +tab-bar--override-doom-buffer-list-a ()
      (if tabspaces-mode
          (advice-add #'doom-buffer-list :override #'+workspace-buffer-list)
        (advice-remove #'doom-buffer-list #'+workspace-buffer-list))))

  (when (fboundp '+doom-dashboard-reload-maybe-h)
    (add-hook! '(tab-bar-tab-post-select-functions
                 tab-bar-tab-post-open-functions)
               #'+doom-dashboard-reload-maybe-h))

  ;; (advice-add #'tabspaces--local-buffer-p :before-while #'doom-buffer-frame-predicate)
  (advice-add #'tabspaces--local-buffer-p :before-while #'doom-real-buffer-p)

  (akn/advice-remove #'projectile-switch-project-by-name :around #'+workspaces--with-permanent-tab-names-a))


;;; bufferlo
(use-package! bufferlo
  :when (modulep! +bufferlo)
  :defer t
  :defer-incrementally (seq tab-bar desktop bookmark ibuffer ibuf-ext)
  :init
  ;; these must be set before the bufferlo package is loaded
  (setq bufferlo-prefer-local-buffers 'tabs)
  :config
  (setq! bufferlo-mode-line nil
         bufferlo-mode-line-prefix "🐃" ; "🐮"
         bufferlo-mode-line-set-active-prefix "Ⓢ"
         bufferlo-mode-line-frame-prefix "Ⓕ"
         bufferlo-mode-line-tab-prefix "Ⓣ"
         bufferlo-mode-line-left-prefix nil
         bufferlo-mode-line-right-suffix nil
         bufferlo-kill-modified-buffers-policy nil ; 'retain-modified-kill-without-file-name nil 'retain-modified 'retain-modified-kill-without-file-name 'kill-modified
         bufferlo-bookmark-inhibit-bookmark-point t
         ;; bufferlo-delete-frame-kill-buffers-prompt t
         bufferlo-bookmark-frame-save-on-delete 'when-bookmarked
         bufferlo-bookmark-tab-save-on-close 'when-bookmarked
         ;; bufferlo-close-tab-kill-buffers-prompt t
         ;; bufferlo-bookmark-frame-load-make-frame 'restore-geometry
         bufferlo-bookmark-frame-load-policy 'prompt
         bufferlo-bookmark-frame-duplicate-policy 'prompt
         bufferlo-bookmark-tab-replace-policy 'prompt ; 'new 'replace
         bufferlo-bookmark-tab-duplicate-policy 'prompt
         bufferlo-bookmark-tab-in-bookmarked-frame-policy 'allow
         bufferlo-bookmark-tab-failed-buffer-policy 'placeholder
         bufferlo-bookmarks-save-duplicates-policy 'prompt
         bufferlo-bookmarks-save-frame-policy 'all
         bufferlo-bookmarks-save-at-emacs-exit 'all
         bufferlo-bookmarks-load-at-emacs-startup 'noload
         bufferlo-bookmarks-auto-save-interval (* 60 2)
         bufferlo-bookmarks-auto-save-messages nil
         bufferlo-set-restore-geometry-policy 'all
         bufferlo-set-restore-tabs-reuse-init-frame 'reuse ; nil 'reuse 'reuse-reset-geometry
         bufferlo-set-restore-ignore-already-active 'prompt ; nil 'prompt 'ignore
         bufferlo-frameset-restore-geometry 'bufferlo
         bufferlo-frame-geometry-function #'bufferlo-frame-geometry-default
         bufferlo-frame-sleep-for (cond ((featurep :system 'macos) 0.05)
                                        ((featurep :system 'linux) 0.35)
                                        (t 0.35)))

  (akn/after-idle! (13 :each-idle t :timer-name akn/bufferlo-autosave-timer)
    (bufferlo-bookmarks-save))

  (setq bookmark-bmenu-type-column-width 12) ; supported in Emacs 31 (innocuous on earlier versions)

  (setq bufferlo-bookmark-buffers-exclude-filters
        (list
         (rx bos " " (1+ anything)) ; ignores "invisible" buffers; e.g., " *Minibuf...", " markdown-code-fontification:..."
         (rx bos "*" (1+ anything) "*") ; ignores "special" buffers; e.g;, "*Messages*", "*scratch*", "*occur*"
         (rx bos "*magit-process:" (1+ anything) "*")))
  (setq bufferlo-bookmark-buffers-include-filters
        (list
         (rx bos "*magit:")
         (rx bos "*magit-log:")
         (rx bos "*shell*") ; comment out shells if you do not have bookmark support
         (rx bos "*" (1+ anything) "-shell*") ; project.el shell buffers
         (rx bos "*eshell*")
         (rx bos "*" (1+ anything) "-eshell*"))) ; project.el eshell buffers

  ;; slow and probably not necessary
  (akn/undefine-advice bufferlo-buffer-list (:around (fn &optional frame tabnum include-hidden &rest args) akn/doom-real-buffer-a)
    (let ((ret (apply fn frame tabnum include-hidden args)))
      (if include-hidden
          ret
        (seq-filter #'doom-real-buffer-p ret))))

  (defvar my:bufferlo-consult--source-local-buffers
    (list :name "Bufferlo Local Buffers"
          :narrow   ?l
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-other-buffers
    (list :name "Bufferlo Other Buffers"
          :narrow   ?o
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-non-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-all-buffers
    (list :name "Bufferlo All Buffers"
          :narrow   ?a
          :hidden   t
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name)))
    "All Bufferlo buffer candidate source for `consult-buffer'.")

  (after! consult
    ;; add in the reverse order of display preference
    (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-all-buffers)
    (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
    (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers))

  (add-hook! 'bufferlo-mode-hook
    (defun +tab-bar--override-doom-buffer-list-h ()
      (if bufferlo-mode
          (advice-add #'doom-buffer-list :override #'+workspace-buffer-list)
        (advice-remove #'doom-buffer-list #'+workspace-buffer-list))))

  (when (fboundp '+doom-dashboard-reload-maybe-h)
    (add-hook! '(tab-bar-tab-post-select-functions
                 tab-bar-tab-post-open-functions)
               #'+doom-dashboard-reload-maybe-h))

  (advice-add #'doom/quicksave-session :override #'+tab-bar:doom-save-session)
  (advice-add #'doom/quickload-session :override #'+workspace/restore-last-session)

  (akn/advise-letf! +vterm/toggle (+tab-bar--bufferlo)
    (tabspaces-mode bufferlo-mode))

  (bufferlo-mode)
  ;; (bufferlo-anywhere-mode) ; this seems to be slow by default
  nil)

;;; sessions

(advice-add #'doom-session-file :override #'+tab-bar:doom-session-file)
(advice-add #'doom-load-session :override #'+tab-bar:doom-load-session)
(advice-add #'doom-save-session :override #'+tab-bar:doom-save-session)

(add-hook 'kill-emacs-query-functions #'+tab-bar:doom-save-session)
  ;; like desktop-save-mode desktop-auto-save-timeout
(defvar +tab-bar--window-configuration-changed-p nil)
(add-hook! 'window-configuration-change-hook
  (setq +tab-bar--window-configuration-changed-p t))
(akn/after-idle! (11 :each-idle t :timer-name +tab-bar--save-session-timer)
  (when +tab-bar--window-configuration-changed-p
    (+tab-bar:doom-save-session)
    (setq +tab-bar--window-configuration-changed-p nil)))

;;; workspaces

(add-hook! 'akn/before-switch-project-hook
  #'+workspaces-switch-to-project-h)

(when (modulep! :completion vertico)
  (map! [remap tabspaces-switch-to-buffer] #'+tab-bar+vertico/switch-workspace-buffer
        [remap bufferlo-switch-to-buffer]  #'+tab-bar+vertico/switch-workspace-buffer))

(defadvice! +tab-bar-act-as-persp-a (fn &rest args)
  :around #'+vterm/toggle
  :around #'+term/toggle
  :around #'+shell/toggle
  :around #'+eshell/toggle
  (akn/letf! ((persp-mode t)
              ;; (safe-persp-name (get-current-persp))
              (#'get-current-persp #'+workspace-current)
              (#'safe-persp-name #'+workspace--name))
    (apply fn args)))

;;; activities
(use-package! activities
  :defer t
  :defer-incrementally cl-lib bookmark map persist subr-x color warnings
  :config
  (setq! activities-default-name-fn #'+workspace-current-name)
  (activities-mode)
  (activities-tabs-mode))
