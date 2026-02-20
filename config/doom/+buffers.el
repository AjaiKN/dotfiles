;;; +buffers.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'general)

(eval-when-compile
  (require 'doom-lib)
  (require 'doom))

(eval-and-compile
  (add-load-path! "./lisp"))

(eval-when-compile
  ;; (require 'doom-packages)
  (require 'akn-doom-use-package)
  ;; (require 'doom-modules)
  (require 'doom-keybinds)
  (require 'subr-x))

(eval-and-compile
  (setq! use-package-always-defer t))

(eval-and-compile
  (require 'akn))

;;; helpers

(defun akn/notmuch-buffer-p (&optional b)
  (and (fboundp #'notmuch-interesting-buffer)
       (notmuch-interesting-buffer (or b (current-buffer)))))

(defun akn/mu4e-buffer-p (&optional b)
  (string-match-p (rx bos "*mu4e") (buffer-name (or b (current-buffer)))))

;;; Window management
;; originally 5 seconds
(setq +popup-defaults
      (list :side   'bottom
            :height 0.16
            :width  40
            :quit   t
            :select #'ignore
            :ttl    #'akn/mark-buffer-for-cleaning))
(defun akn/mark-buffer-for-cleaning (buf)
  (after! midnight
    (when (buffer-live-p buf)
      (add-to-list 'clean-buffer-list-kill-buffer-names (buffer-name buf)))))
(set-popup-rule! (rx bol "*doom:scratch") :size 0.25 :side 'right)
(after! ielm (set-popup-rule! (rx bol "*ielm") :size 10 :vslot -8 :select t :quit #'akn/insert-state-and-close-popup-h :ttl nil))
;; not working for term right now, but the others work
(after! (:or term vterm shell eshell eat mistty)
  (set-popup-rule! (rx (or (seq bol "*doom:" (or "term" "vterm" "shell" "eshell") "-popup:")
                           (seq (or "*" "-") (or "eat" "mistty") "*")
                           (seq "*vterm")))
    :vslot -5 :size 0.25 :select t :modeline nil :quit #'akn/insert-state-and-close-popup-h :ttl nil))
(defun akn/insert-state-and-close-popup-h (window)
  (when (modulep! :editor evil)
    (shut-up
      (with-selected-window window
        (evil-append 1))))
  t)
(akn/advise-letf! +term/toggle (akn/a)
  (defadvice! akn/akn/+term/toggle/display-buffer-in-side-window-a (buffer _alist)
    :override #'display-buffer-in-side-window
    (progn
      (advice-remove #'display-buffer-in-side-window #'akn/akn/+term/toggle/display-buffer-in-side-window)
      (display-buffer buffer))))

(after! timer-list (set-popup-rule! (rx "*timer-list*") :height 15))

(set-popup-rule! '(major-mode . profiler-report-mode) :quit nil)

;; not sure (see `auto-revert-mode' docs)
(setq!
 ;; default: `revert-buffer-insert-file-contents--default-function'
 revert-buffer-insert-file-contents-function #'revert-buffer-insert-file-contents-delicately
 ;; default: 2.0
 revert-buffer-with-fine-grain-max-seconds 0.5)

;; reload buffer
(map! "s-r" #'akn/reload-buffer
      "s-R" #'akn/hard-reload-buffer)
(defun akn/reload-buffer (&optional even-if-modified allow-mode-change undelicately)
  "Revert the current buffer.

If EVEN-IF-MODIFIED is non-nil, revert the buffer even if it's
modified. Interactively, use a prefix argument.

If ALLOW-MODE-CHANGE is non-nil, reinitialize the modes.
Interactively, running this command twice in a row will do that.

If UNDELICATELY is non-nil, and
`revert-buffer-insert-file-contents-function' is
`revert-buffer-insert-file-contents-delicately', set it to
`revert-buffer-insert-file-contents--default-function' instead,
which is faster.
Interactively, run \\[universal-argument] \\[universal-argument] \\[akn/reload-buffer]."
  (interactive (list current-prefix-arg
                     (or (equal current-prefix-arg '(16))
                         (eq last-command #'akn/reload-buffer))
                     (equal current-prefix-arg '(16))))
  (let ((revert-buffer-insert-file-contents-function
         (if (and (eq revert-buffer-insert-file-contents-function #'revert-buffer-insert-file-contents-delicately)
                  undelicately)
             #'revert-buffer-insert-file-contents--default-function
           revert-buffer-insert-file-contents-function)))
    (cond
     ((derived-mode-p 'org-agenda-mode)
      (org-agenda-redo))
     ((derived-mode-p 'helpful-mode)
      (message "Updating helpful-mode buffer")
      (helpful-update)
      (message "Updating helpful-mode buffer...done"))
     ((derived-mode-p 'Info-mode)
      (message "Updating Info-mode buffer...")
      (Info-revert-buffer-function nil t))
     ((derived-mode-p 'treemacs-mode)
      (message "Refreshing treemacs...")
      (dlet ((treemacs-silent-refresh nil))
        (treemacs-refresh)))
     ((derived-mode-p 'elfeed-search-mode)
      (message "Updating elfeed-search-mode buffer...")
      (call-interactively #'elfeed-search-update--force)
      (message "Updating elfeed-search-mode buffer...done"))
     ((derived-mode-p 'stgit-mode)
      (stgit-reload "Reloading stgit-mode buffer")
      (message "Reloading stgit-mode buffer...done"))
     ((akn/notmuch-buffer-p)
      (message "Refreshing notmuch buffer...")
      (call-interactively #'notmuch-refresh-this-buffer)
      (message "Refreshing notmuch buffer...done"))
     ((derived-mode-p 'xwidget-webkit-mode)
      (call-interactively #'xwidget-webkit-reload))
     ((and (eq revert-buffer-function #'revert-buffer--default)
           (or (null buffer-file-name)
               (and (not even-if-modified) (buffer-modified-p))))
      (message "Running `normal-mode'...")
      (let ((old-major-mode major-mode)
            actual-normal-mode
            (file-was-modified (and buffer-file-name (not even-if-modified) (buffer-modified-p))))
        (normal-mode t)
        (setq actual-normal-mode major-mode)
        (unless allow-mode-change
          (funcall old-major-mode))
        (message "%s%s"
                 (if file-was-modified
                     (progn
                       (ding)
                       (format-message "`%s' is modified! Use with C-u to actually revert"
                                       (file-name-nondirectory buffer-file-name)))
                   (format-message "Running `normal-mode'...done"))
                 (cond ((not (derived-mode-p actual-normal-mode))
                        (format-message " (reload again to change to `%s')"
                                        actual-normal-mode))
                       ((not (eq old-major-mode major-mode))
                        (format-message " (changed to `%s', was `%s')." major-mode old-major-mode))
                       (t "")))))
     (t
      (when (and (not even-if-modified)
                 (memq revert-buffer-function (list #'revert-buffer--default 'vlf-revert #'+mediawiki-revert-buffer-fn))
                 (buffer-modified-p))
        (user-error "Buffer modified, not reverting"))
      (message "Reverting `%s' buffer%s%s..."
               major-mode
               (if allow-mode-change "" " finely")
               (if (eq revert-buffer-function #'revert-buffer--default)
                   ""
                 (format-message " (`%s')" revert-buffer-function)))
      (akn/revert-buffer-keep-read-only t
                                        (or even-if-modified
                                            ;; (when (eq revert-buffer-function 'vlf-revert)
                                            (not (buffer-modified-p)))
                                        (not allow-mode-change))
      (message "Reverting `%s' buffer%s%s...done"
               major-mode
               (if allow-mode-change "" " finely")
               (if (eq revert-buffer-function #'revert-buffer--default)
                   ""
                 (format-message " (`%s')" revert-buffer-function)))))
    (akn/record-buffer)))
(defun akn/hard-reload-buffer (&optional even-if-modified)
  (interactive (list current-prefix-arg))
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (org-agenda-redo t))
   ((akn/notmuch-buffer-p)
    (notmuch-poll)
    (notmuch-refresh-all-buffers))
   ((derived-mode-p 'stgit-mode)
    (call-interactively #'stgit-repair))
   ((and (not even-if-modified) (buffer-modified-p))
    (akn/reload-buffer nil t t))
   (t
    (if buffer-file-name
        (let ((the-filename buffer-file-name)
              (buf (current-buffer))
              (temp-buf (generate-new-buffer "*temp-reloading-buffer*")))
          (unwind-protect
              (with-window-non-dedicated nil
                (set-window-buffer nil temp-buf)
                (message "Killing buffer...")
                (if even-if-modified
                    (progn
                      (when (buffer-modified-p) (do-auto-save nil t))
                      (set-buffer-modified-p nil)
                      (let ((kill-buffer-query-functions))
                        (kill-buffer buf)))
                  (kill-buffer buf))
                (message "Reopening file...")
                (set-window-buffer nil (find-file-noselect the-filename))
                (message "Reopening file...done"))
            (with-temp-message "Killing temp buffer..."
              (kill-buffer temp-buf))))
      (akn/reload-buffer nil t t))))
  (akn/record-buffer))

(defun akn/revert-buffer-keep-read-only (&optional ignore-auto noconfirm preserve-modes)
  "Like `revert-buffer', except preserves `buffer-read-only' if PRESERVE-MODES.

With `revert-buffer', if `after-find-file' normally resets
`buffer-read-only'. If PRESERVE-MODES is nil, this isn't
generally a problem for me, since `after-change-major-mode-hook'
will still get run, which will enable my
`akn/read-only-project-mode' if necessary. So if PRESERVE-MODES
is non-nil, this function will preserve `buffer-read-only' if it
was non-nil."
  (let ((was-read-only buffer-read-only))
    (prog1
        (funcall #'revert-buffer ignore-auto noconfirm preserve-modes)
      (when (and preserve-modes was-read-only)
        (setq-local buffer-read-only t)))))

(defadvice! akn/open-outside-of-popup-a (fn &rest args)
  :around #'+lookup/definition
  :around #'+lookup/file
  :around #'+lookup/references
  :around #'+lookup/implementations
  :around #'+lookup/type-definition
  (if (and (+popup-window-p)
           (null (car display-buffer-overriding-action)))
      (let ((switch-to-buffer-obey-display-actions t)
            (display-buffer-overriding-action
             (cons (lambda (buffer _alist)
                     (let ((window (save-popups! (selected-window)))
                           switch-to-buffer-obey-display-actions
                           display-buffer-overriding-action)
                       (select-window window)
                       (switch-to-buffer buffer nil t)
                       t))
                   nil)))
        (apply fn args))
    (apply fn args)))

(defun akn/make-frame-other-monitor ()
  (interactive)
  (let* ((current-monitor (alist-get 'name (frame-monitor-attributes)))
         (monitors (or (delq nil (mapcar (lambda (a)
                                           (alist-get 'name a))
                                         (display-monitor-attributes-list)))
                       '("")))
         (other-monitors (remove current-monitor monitors))
         (monitor (car other-monitors)))
    (if (eq (length other-monitors) 1)
        (make-frame-on-monitor monitor)
      (call-interactively #'make-frame-on-monitor))))

(defun akn/other-monitor-prefix ()
  ;; see `other-frame-prefix'
  "TODO")

(defun akn/popup-as-side-bar ()
  (interactive)
  (let ((+popup-defaults
         ;; Read about these options in the docs for `set-popup-rule!'
         (append '(:side left
                   :width 0.2
                   :quit nil
                   :ttl nil)
                 +popup-defaults)))
    (+popup/buffer)))

(add-hook! 'follow-mode-hook
  (defun akn--follow-mode-no-smooth-scroll-h ()
    (if follow-mode
        (akn/local-smooth-scroll-disabled-mode)
      (akn/local-smooth-scroll-disabled-mode -1))))

;;; workspaces

(map! [remap +workspace/save] #'akn/+workspace/save)
(defun akn/+workspace/save (name)
  "Save the current workspace. If called with C-u, ask which workspace.

This is just like `+workspace/save', except the meaning of the
prefix argument is reversed."
  (interactive
   (list
    (if current-prefix-arg
        (completing-read "Workspace to save: " (+workspace-list-names))
      (+workspace-current-name))))
  (+workspace/save name))

;; (akn/undefine-advice doom-fallback-buffer (:override () akn/make-project-dired-the-fallback-buffer-a)
;;   (let (buffer-list-update-hook)
;;     (if-let* ((pwd (and (bound-and-true-p persp-mode)
;;                         (persp-parameter 'last-project-root))))
;;         (or (dired-find-buffer-nocreate pwd)
;;             (save-current-buffer
;;               (save-window-excursion
;;                 (dired pwd)
;;                 (current-buffer))))
;;       (get-buffer-create doom-fallback-buffer-name))))

;;; winner-mode
(map! "C-c <left>" #'winner-undo
      "C-c <right>" #'winner-redo)

;;; new buffers
(defvar akn/default-major-mode #'fundamental-mode)

(map! [remap +default/new-buffer] #'akn/new-buffer)
(defun akn/new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (akn/new-file-mode)
      (funcall (or akn/default-major-mode (default-value 'major-mode)))
      (akn/mark-buffer-real))
    buffer))

(defadvice! akn/new-buffer-is-not-package-buffer-a (&rest _)
  :before-while #'+emacs-lisp--in-package-buffer-p
  (akn/maybe-disable-new-file-mode)
  (not akn/new-file-mode))

(define-minor-mode akn/new-file-mode
  "Allow this buffer to be saved, even if it's still empty and has
no file name."
  :group 'akn
  :keymap (make-sparse-keymap)
  (akn/mode-set akn/new-file-mode
    buffer-offer-save t
    doom-real-buffer-p t)
  (setq-local lexical-binding t)
  (add-hook! 'after-save-hook :local #'akn/maybe-disable-new-file-mode)
  (akn/mark-buffer-real))
(put 'akn/new-file-mode 'permanent-local t)
(add-hook 'after-change-major-mode-hook #'akn/new-file-mode-reenable-h)
(defun akn/new-file-mode-reenable-h ()
  (when (and akn/new-file-mode
             (not buffer-file-name))
    (akn/new-file-mode)))
(map! :map akn/new-file-mode-map
      [remap save-buffer] #'akn/save-buffer-even-if-empty
      (:localleader
       "e" #'emacs-lisp-mode
       "l" #'common-lisp-mode
       "t" #'text-mode
       "m" #'markdown-mode
       "o" #'org-mode
       "r" #'ruby-mode
       "R" #'ess-r-mode
       "C-r" #'roc-ts-mode
       "s" #'sh-mode
       "b" #'sh-mode
       "p" #'python-mode))

(defun akn/add-lexical-binding ()
  (save-excursion
    (goto-char 1)
    (unless (search-forward "lexical-binding: t" nil t)
      (goto-char 1)
      (insert ";;; -*- lexical-binding: t; -*-\n"))))
(add-hook! 'emacs-lisp-mode-hook
  (defun akn/add-lexical-binding-h ()
    (when (and akn/new-file-mode (not buffer-read-only))
      (akn/add-lexical-binding))))

(defun akn/save-buffer-even-if-empty (&optional arg)
  "Just like `save-buffer', except that if the buffer has no file
name and is empty, it'll still try to save it."
  (interactive "p")
  (if (or (buffer-modified-p)
          (> (buffer-size) 0)
          buffer-file-name)
      (save-buffer arg)
    (set-buffer-modified-p t)
    (unwind-protect
        (save-buffer arg)
      (set-buffer-modified-p nil))))

(defun akn/maybe-disable-new-file-mode ()
  (when (and akn/new-file-mode buffer-file-name)
    (akn/new-file-mode -1)))

;;; framemove
(use-package! framemove
  :defer-incrementally t
  :after windmove
  :demand t
  :config
  ;; make windmove also be able to move to frame
  (setq! framemove-hook-into-windmove t)

  (defadvice! akn/framemove--should-be-user-error-a (fn &rest args)
    :around #'fm-next-frame
    (condition-case err
        (apply fn args)
      (error
       (if (equal (car-safe (cdr-safe err))
                  "No frame in that direction")
           (user-error "No frame in that direction")
         (signal (car err) (cdr err)))))))

;;; undelete-frame-mode

(undelete-frame-mode)
(defalias 'akn/undo-delete-frame #'undelete-frame)
(defalias 'akn/undo-close-frame #'undelete-frame)

(define-advice doom/delete-frame-with-prompt (:override () akn/a)
  "Delete the current frame, but ask for confirmation if it isn't empty."
  (interactive)
  (if (cdr (frame-list))
      (when (or (not confirm-kill-emacs)
                (doom-quit-p "Close frame?"))
        (delete-frame))
    (save-buffers-kill-emacs)))

;;; file-local variables

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
