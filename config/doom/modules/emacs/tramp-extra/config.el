;;; emacs/tramp-extra/config.el -*- lexical-binding: t; -*-

(require 'akn)

(after! (:or emacs tramp)
  (setq tramp-completion-use-auth-sources nil))

(after! tramp
  ;; https://www.gnu.org/software/emacs/manual/html_mono/tramp.html#Using-ssh-config-include-for-host-name-completion
  (tramp-set-completion-function
   "ssh" (append (tramp-get-completion-function "ssh")
                 (mapcar (lambda (file) `(tramp-parse-sconfig ,(expand-file-name file)))
                         (doom-glob "~/.ssh/config.*"))))

  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-06/msg00810.html
  (setq! tramp-inhibit-errors-if-setting-file-attributes-fail t))

;;; doom bugfixes

(setq
 ;; TODO: PR or issue: by default, Doom sets it to an absolute path
 ;; ("/Users/ajainelson/.config/emacs/.local/cache/backup/"), which doesn't
 ;; exist on Linux. Maybe use `abbreviate-file-name'.
 tramp-backup-directory-alist (list (cons "." "~/.cache/doom/tramp-backup/"))
 tramp-auto-save-directory "~/.cache/doom/tramp-autosave/")

(after! doom-editor
  ;; TODO: PR or issue
  (defadvice! doom-make-hashed-auto-save-file-name-a (fn)
    "Compress the auto-save file name so paths don't get too long."
    :around #'make-auto-save-file-name
    (let ((buffer-file-name
           (if (or
                ;; Don't do anything for non-file-visiting buffers. Names
                ;; generated for those are short enough already.
                (null buffer-file-name)
                ;; If an alternate handler exists for this path, bow out. Most of
                ;; them end up calling `make-auto-save-file-name' again anyway, so
                ;; we still achieve this advice's ultimate goal.
                (find-file-name-handler buffer-file-name
                                        'make-auto-save-file-name))
               buffer-file-name
             ;;CHANGED
             (concat (or (and (string-match-p "/tramp-autosave/" buffer-file-name)
                              (file-name-directory buffer-file-name))
                         "")
                     (sha1 buffer-file-name)))))
      (funcall fn))))

;;; PERF: tramp

(after! files-x
  ;; This will cause errors if the commands tramp sends are too long.
  ;; But it makes stuff (e.g. M-x compile) a lot faster.
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "sshx")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "sshx")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "rsync")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "rclone")
   'remote-direct-async-process))

(setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))

(after! (:or emacs files tramp)
  ;; method performance IME:
  ;; 1. rsync
  ;; 2. scpx
  ;; 3. sshx
  (unless (featurep :system 'windows)
    (setq tramp-default-method "rsync"))
  (setq remote-file-name-access-timeout 5
        remote-file-name-inhibit-cache (* 60 10)))

(after! shell
  (define-advice shell-mode (:around (fn &rest args) akn/disable-remote-shell-history-a)
    (if (akn/dir-slow-p default-directory)
        (let ((shell-history-file-name t))
          (apply fn args))
      (apply fn args))))

(after! tramp
  (when (file-exists-p "~/.ssh/config")
    ;; This check is very imperfect. It doesn't check that it's under "Host *", for example.
    (when (with-file-contents! "~/.ssh/config"
            (let ((case-fold-search t))
              (or
               (and (save-excursion (search-forward-regexp (rx bol (* blank)
                                                               "ControlMaster"
                                                               (or (+ blank) (seq (* blank) "=" (* blank)))
                                                               (or "auto" "yes"))
                                                           nil t))
                    (save-excursion (search-forward-regexp (rx bol (* blank)
                                                               "ControlPersist"
                                                               (or (+ blank) (seq (* blank) "=" (* blank)))
                                                               (or "yes" digit "."))
                                                           nil t))
                    (save-excursion (search-forward-regexp (rx bol (* blank)
                                                               "ControlPath"
                                                               (or (+ blank) (seq (* blank) "=" (* blank)))
                                                               ;; "It is recommended that any ControlPath used for opportunistic connection sharing include at least %h, %p, and %r (or alternatively %C)"
                                                               ;; - man 5 ssh_config
                                                               (or (seq (* nonl) "%C")
                                                                   (seq (* nonl) "%" (any "rhp") (* nonl) "%" (any "rhp") (* nonl) "%" (any "rhp"))))
                                                           nil t)))
               ;; "tramp-use-connection-share should also be set to nil or suppress if you use the ProxyCommand or ProxyJump options in your ssh configuration."
               (save-excursion
                 (when (file-exists-p "~/.ssh/config.local")
                   (save-excursion
                     (goto-char (point-max))
                     (insert "\n")
                     (insert-file-contents-literally "~/.ssh/config.local")))
                 (search-forward-regexp (rx bol (* blank) "Proxy") nil t)))))
      (setq! tramp-use-connection-share nil))))


;; https://discord.com/channels/406534637242810369/406624667496087572/1349607536235380780
(after! projectile
  (setq projectile-generic-command
        (if projectile-fd-executable
            (format "%s . -0 --type f --color=never --strip-cwd-prefix" projectile-fd-executable)
          "find . -type f | cut -c3- | tr '\\n' '\\0'")))

;; from Doom: add `tramp-file-name-regexp' to `vc-ignore-dir-regexp'
(after! (:or vc-hooks tramp)
  (setq vc-ignore-dir-regexp
        (rx (or (regexp locate-dominating-stop-dir-regexp)
                (regexp tramp-file-name-regexp)
                (seq (any ?/ ?\\) "node_modules")))))

;; not sure whether this is helping
(after! editorconfig
  (when (boundp 'editorconfig-exclude-regexps)
    (add-to-list 'editorconfig-exclude-regexps akn/file-remote-regexp)))

;; magit-wip-mode is super slow with tramp
(defadvice! akn/magit-wip--only-if-not-tramp-a (&rest _)
  :before-while #'magit-wip-commit-initial-backup
  :before-while #'magit-wip-after-save-local-mode-turn-on
  :before-while #'magit-wip-commit-buffer-file
  (not (and buffer-file-name
            (akn/file-remote-p buffer-file-name))))

;;; PERF: speed up projectile over tramp

;; NOTE: these tramp projectile performance issues aren't as big with project.el

(after! projectile
  (define-advice projectile-project-root (:around (fn &optional dir &rest args) akn/tramp-speed-up-a)
    (setq dir (or dir default-directory))
    (let ((ret
           (if (akn/dir-slow-p dir)
               ;; see `projectile-project-root-functions'
               (let ((projectile-project-root-functions (list #'akn/projectile-root-assume-cached
                                                              #'projectile-root-local
                                                              #'projectile-root-marked
                                                              #'projectile-root-bottom-up))
                     (projectile-project-root-files-bottom-up '(".git"))
                     (projectile-project-root-files '())
                     (projectile-project-root-files-top-down-recurring '()))
                 ;; (message "hi")
                 (apply fn dir args))
               ;; nil
               ;; (let ((project-find-functions (remove #'project-projectile project-find-functions)))
               ;;   (when-let* ((proj (project-current)))
               ;;     (project-root proj)))
             (apply fn dir args))))
      (when (and ret
                 (not (member (file-name-as-directory (abbreviate-file-name ret)) projectile-known-projects)))
        (with-demoted-errors "projectile-project-root@akn/tramp-speed-up-a: %S"
          (projectile-add-known-project ret)))
      ret)))

(after! projectile
  (defun akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a (fn file name &rest args)
    :around #'projectile-locate-dominating-file
    (if-let* (((stringp file))
              ((akn/file-remote-p file))
              (home (ignore-errors (akn/tramp-home-dir file))))
        (let* ((locate-dominating-stop-dir-regexp
                (rx (or (regexp locate-dominating-stop-dir-regexp)
                        (seq bos (literal home) (? "/") eos)))))
          (apply fn file name args))
      (apply fn file name args)))
  (advice-add #'projectile-locate-dominating-file :around #'akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a))
(after! project
  (akn/advise-letf! project-try-vc (akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a)
    (advice-add #'locate-dominating-file :around #'akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a)))

;; not sure whether this is actually helping
(defun akn/known-project-roots-above (&optional file)
  (setq file (or file buffer-file-name default-directory))
  (let ((home-dir (concat (akn/tramp-home-dir file) "/")))
    (cl-loop for root in projectile-known-projects ; (project-known-project-roots)
             when (tramp-equal-remote root file)
             when (string-prefix-p (replace-regexp-in-string (rx "~/") home-dir root) file)
             collect root)))
(defun akn/projectile-root-assume-cached (&optional file)
  (setq file (or file buffer-file-name default-directory))
  (let ((possible-project-roots (akn/known-project-roots-above)))
    (unless (length= possible-project-roots 1)
      (doom-log "failed"))
    (when (length= possible-project-roots 1)
      (doom-log "got it")
      (car possible-project-roots))))
(defun akn/projectile-invalidate-cached-project-roots (&optional file)
  (interactive)
  (setq file (or file buffer-file-name default-directory))
  (projectile-invalidate-cache nil)
  (setq-local projectile-project-root nil)
  (dotimes (_ 2)
    (cl-loop for root in (seq-union (akn/known-project-roots-above file)
                                    (let ((projectile-known-projects (project-known-project-roots)))
                                      (akn/known-project-roots-above file)))
             do
             (projectile-remove-known-project root)
             (project-forget-project root)
             (projectile-remove-known-project root)))
  (setq-local projectile-project-root nil)
  (projectile-invalidate-cache nil)
  (message "%s" (projectile-project-root)))

(defadvice! akn/doom-project-ignored-p--a (project-root)
  :override #'doom-project-ignored-p
  (when (string-match-p (rx bos "~" (or "/" eos)) project-root)
    (setq project-root (concat (getenv-internal "HOME") (substring-no-properties project-root 1))))
  (if (akn/file-remote-p project-root)
      (or (file-in-directory-p project-root temporary-file-directory)
          (file-in-directory-p project-root doom-local-dir))
    (or (string-prefix-p temporary-file-directory project-root)
        (string-prefix-p doom-local-dir project-root))))
