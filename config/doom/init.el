;;; init.el -*- lexical-binding: t; -*-

;; NOTE: Machine-specific settings should be placed in local.el (gitignored).

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!
;;                     ^^^^^^^^^^^ or SPACE h r r

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

;; Doom disables these startup optimizations on daemons, since startup time
;; doesn't matter as much. But I still care about startup time with the daemon,
;; so I'm restoring some of the simpler, more impactful ones.
(when (daemonp)
  (unless noninteractive
    (setq frame-inhibit-implied-resize t)
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name
          initial-major-mode 'fundamental-mode
          initial-scratch-message nil)
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)
    (define-advice setopt--set (:around (fn &rest args) inhibit-load-symbol -90)
      (let ((custom-load-recursion t))
        (apply fn args)))))

(add-load-path! "./lisp")

(setq el-patch-use-advice t)
(setq evil-collection-setup-minibuffer t)

(defvar akn/private-doom-dir "~/.config/doom.private/")

(after! doom-start
  (when (not (fboundp 'gcmh-mode))
    ;; Don't enable gcmh-mode if the package isn't installed
    (remove-hook 'doom-first-buffer-hook 'gcmh-mode)))

;; can remove this paragraph after Doom 3
(after! (:or emacs doom doom-projects)
  (require 'xdg)
  (setq! doom-data-dir
         (if (featurep :system 'windows)
             (expand-file-name "doomemacs/data/" (getenv-internal "LOCALAPPDATA"))
           (expand-file-name "doom/" (xdg-data-home)))
         doom-cache-dir
         (if (featurep :system 'windows)
             (expand-file-name "doomemacs/cache/" (getenv-internal "LOCALAPPDATA"))
           (expand-file-name "doom/" (xdg-cache-home)))
         doom-state-dir
         (if (featurep :system 'windows)
             (expand-file-name "doomemacs/state/" (getenv-internal "LOCALAPPDATA"))
           (expand-file-name "doom/" (xdg-state-home)))
         doom-profile-cache-dir    (file-name-concat doom-cache-dir (car doom-profile))
         doom-profile-data-dir     (file-name-concat doom-data-dir (car doom-profile))
         doom-profile-state-dir    (file-name-concat doom-state-dir (car doom-profile))
         doom-profile-dir          (file-name-concat doom-profile-data-dir "@" (cdr doom-profile))
         desktop-dirname           (file-name-concat doom-profile-state-dir "desktop")
         pcache-directory          (file-name-concat doom-profile-cache-dir "pcache/")
         doom-projectile-cache-dir (file-name-concat doom-profile-cache-dir "projectile/")
         doom-sync-info-file       (file-name-concat doom-profile-data-dir "sync")
         project-list-file         (file-name-concat doom-profile-state-dir "projects")
         doom-store-dir (concat doom-data-dir "store/")
         doom-profiles-generated-dir doom-data-dir
         doom-cli-log-file-format (expand-file-name "logs/cli.%s.%s.%s" doom-state-dir))
  (when (boundp 'doom-local-dir)
    (require 'cl-lib)
    (cl-loop for (target source) in (list
                                     (list doom-data-dir (file-name-concat doom-local-dir "etc"))
                                     (list doom-cache-dir (file-name-concat doom-local-dir "cache"))
                                     (list doom-state-dir (file-name-concat doom-local-dir "state")))
             do
             (mkdir target 'parents)
             (unless (equal (file-symlink-p source) target)
               (when (file-exists-p source)
                 (message "Trashing %s" source)
                 (move-file-to-trash source))
               (message "ln -s %s %s" target source)
               (make-symbolic-link target source)))))
(after! doom
  (setq user-emacs-directory doom-profile-cache-dir))

(defvar akn/terminal-daemon-p
  (equal (daemonp) "term"))
(defvar akn/launched-from-terminal-p
  (and (not (daemonp))
       (not initial-window-system)
       (not window-system)
       (not (display-graphic-p))))
(defvar akn/terminal-only-p
  (or akn/terminal-daemon-p
      akn/launched-from-terminal-p))

;; this will only run if we're running the "doom env" command
(after! doom-cli-env
  ;; Make sure Doom doesn't inherit environment variables that it shouldn't.
  ;; NOTE: These are regexes.
  (pushnew! doom-env-deny
            "ITERM"
            "TERM"
            "MISE"
            "RUBYLIB"
            "JAVA_HOME"
            "_P9K_SSH_TTY"
            "P9K_TTY"
            "TTY"
            "TMPDIR")
  (pushnew! doom-env-allow
            "COLORTERM"
            ;; Otherwise, on nix-darwin, /etc/zshenv will be run again (specifically, /nix/store/*-set-environment is sourced), and it'll reset $PATH.
            ;; In non-interactive shells, ~/.zshrc won't be run, so $PATH won't include everything I need.
            "__NIX_DARWIN_SET_ENVIRONMENT_DONE"
            "__ETC_PROFILE_NIX_SOURCED"
            "\\<NIX\\>"))

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

(load! "+modules")

(define-advice straight--process-output (:filter-return (ret) akn/a)
  "Not sure whether this warning is actually a problem.

Does it mean I should disable core.untrackedCache even though
`git update-index --test-untracked-cache' worked on this system?"
  (if (equal ret "warning: untracked cache is disabled on this system or location\n")
      (progn (warn "%s" ret)
             "")
    ret))

(defvar +ligatures-extra-symbols)
(setq +ligatures-extra-symbols
      '(;; my stuff
        :sum             "∑"
        :product         "∏"
        :ceil            "⌈"
        :floor           "⌊"
        ;; org
        :name            "»"
        :src_block       "»"
        :src_block_end   "«"
        :quote           "“"
        :quote_end       "”"
        ;; Functional
        ;; λx 𝛌x 𝜆x \x 𝝀x 𝝺x 𝞴x
        :lambda          "λ"
        ;; :def          "ƒ"
        ;; :composition  "∘"
        ;; :map          "↦"
        ;; Types
        :null            "∅"
        ;; :true         "𝕋"
        ;; :false        "𝔽"
        ;; :int          "ℤ"
        ;; :float        "ℝ"
        ;; :str          "𝕊"
        ;; :bool         "𝔹"
        ;; :list         "𝕃"
        ;; Flow
        :not             "￢"
        :in              "∈"
        :not-in          "∉"
        :and             "∧"
        :or              "∨"
        :for             "∀"
        :some            "∃"
        ;; :return       "⟼"
        ;; :yield        "⟻"
        ;; Other
        :union           "⋃"
        :intersect       "∩"
        :diff            "∖"
        ;; :tuple        "⨂"
        ;; :dot          "•"
        :pipe            "")) ; FIXME: find a non-private char

;;; restarting emacs

(defvar restart-emacs-daemon-with-tty-frames-p)
(setq restart-emacs-daemon-with-tty-frames-p t)

(defconst akn/restart-emacs-file (file-name-concat doom-cache-dir "emacs-restarting.el"))
(defun akn/restart-emacs-and-then (lisp-form)
  (with-file! akn/restart-emacs-file
    (insert ";;; restarting emacs -*- lexical-binding: t; -*-\n")
    (print lisp-form (current-buffer)))
  (doom/restart))
;; (defun akn/restart ()
;;   (interactive)
;;   (akn/restart-emacs-and-then
;;    `(add-transient-hook! 'doom-after-init-hook
;;       (when (and (daemonp) (not (bound-and-true-p akn/terminal-daemon-p)))
;;         (start-process "emacs-open" nil "emacs-open")))))
(defun akn/restart ()
  (interactive)
  (save-some-buffers nil t)
  (akn/letf! ((#'save-buffers-kill-emacs #'kill-emacs)
              (confirm-kill-emacs nil))
    (akn/restart-emacs-and-then
     `(if (and (daemonp) (not (bound-and-true-p akn/terminal-daemon-p)))
        (start-process "emacs-open-frame" nil "emacs-open-frame")))))
(defun akn/restart-and-restore ()
  (interactive)
  (if (not (seq-some #'doom-real-buffer-p (buffer-list)))
      (akn/restart)
    (doom/quicksave-session)
    (save-some-buffers nil t)
    (akn/letf! ((#'save-buffers-kill-emacs #'kill-emacs)
                (confirm-kill-emacs nil))
      (akn/restart-emacs-and-then
       (let* ((pos (frame-position))
              (thing `(run-with-timer 0 nil
                       (lambda ()
                         (doom-require 'doom-lib 'sessions)
                         (if ',(akn/fullscreenp)
                             (akn/fullscreen-on)
                           (akn/fullscreen-off
                            (and ',(frame-pixel-height)
                                 ',(frame-pixel-height)
                                 (set-frame-size nil ',(frame-pixel-height) ',(frame-pixel-height) 'pixelwise))
                            (and ',(car pos)
                                 ',(cdr pos)
                                 (set-frame-position (selected-frame) ',(car pos) ',(cdr pos))))
                          (set-frame-parameter nil 'alpha ',(frame-parameter nil 'alpha))
                          (set-frame-parameter nil 'alpha-background ',(frame-parameter nil 'alpha-background))
                          (doom/quickload-session 'force))))))
          `(unless (bound-and-true-p akn/terminal-daemon-p)
             (if (daemonp)
                 ;; (add-hook 'window-setup-hook #'doom-load-session 100)
                 (add-transient-hook! 'after-make-frame-functions
                   :after
                   ,thing)
               ,thing)
            (start-process "emacs-open-frame" " *emacs-open-frame*" "emacs-open-frame")))))))
(global-set-key [remap doom/restart] #'akn/restart)
(when (file-exists-p akn/restart-emacs-file)
  (if (condition-case-unless-debug err
          (progn
            (load akn/restart-emacs-file nil 'nomessage)
            t)
        (error
         (display-warning 'akn/restart-emacs-file
                          (format-message "error when loading `%s.el': %S" akn/restart-emacs-file err)
                          :error)
         nil))
      (delete-file akn/restart-emacs-file)
    (rename-file akn/restart-emacs-file
                 (file-name-concat doom-cache-dir "emacs-restarting-FAILED.el")
                 'ok-if-already-exists)))
