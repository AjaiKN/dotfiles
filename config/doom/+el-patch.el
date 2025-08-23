;;; +el-patch.el -*- lexical-binding: t; -*-

(require 'el-patch)

(eval-when-compile
  (require 'doom-lib)
  (require 'doom)
  ;; (require 'doom-packages)
  (require 'akn-doom-use-package)
  ;; (require 'doom-modules)
  (require 'doom-keybinds)
  (require 'subr-x))

(eval-and-compile
  (setq! use-package-always-defer t))

(eval-and-compile
  (require 'akn))

;;; patches
;;;; evil-mc
(use-package! evil-mc-cursor-make
  :config/el-patch
  (el-patch-defun evil-mc-make-cursor-move-by-line (dir count)
    "Create COUNT cursors one for each line moving in the direction DIR.
DIR should be 1 or -1 and COUNT should be a positive integer or nil."
    (el-patch-add
      (require 'evil-mc))
    (evil-force-normal-state)
    (setq count (max 0 (or count 1)))
    (el-patch-wrap 2
      (let ((my-temporary-goal-column (current-column)))
        (dotimes ((el-patch-swap i _i) count)
          (evil-mc-run-cursors-before)
          (evil-mc-make-cursor-at-pos (point))
          (el-patch-add
            (when (null temporary-goal-column) (setq temporary-goal-column my-temporary-goal-column)))
          (el-patch-swap
            (let ((line-move-visual t)) ; not sure why evil-mc does this
              (evil-line-move dir))
            (evil-line-move dir)))))))

;;;; ws-butler
(defvar ws-butler-saved) ;to silence warning
(use-package! ws-butler
  :config/el-patch
  ;; TODO: PR
  (el-patch-defun ws-butler-before-save ()
    "Trim white space before save.
Respects `ws-butler-keep-whitespace-before-point', which see."
    ;; save data to restore later
    (when ws-butler-keep-whitespace-before-point
      (ws-butler-with-save
       (widen)
       (setq ws-butler-presave-coord
             (list (line-number-at-pos (point))
                   (current-column)
                   (el-patch-add
                     (and cursor-type
                          (not (eq (or (car-safe cursor-type) cursor-type) 'bar))
                          (or (eq (char-after) ?\s)
                              (eq (char-after) ?\t))))))))
    (let (last-end)
      (ws-butler-map-changes
       (lambda (_prop beg end)
         (save-excursion
           (setq beg (progn (goto-char beg)
                            (line-beginning-position))
                 ;; Subtract one from end to overcome Emacs bug #17784, since we
                 ;; always expand to end of line anyway, this should be OK.
                 end (progn (goto-char (1- end))
                            (line-end-position))))
         (when (funcall ws-butler-trim-predicate beg end)
           (ws-butler-clean-region beg end))
         (setq last-end end)))
      (ws-butler-maybe-trim-eob-lines last-end)))
  (el-patch-defun ws-butler-after-save ()
    "Restore trimmed whitespace before point."

    (ws-butler-clear-properties)
    ;; go to saved line+col
    (when ws-butler-presave-coord
      (let (remaining-lines)
        (ws-butler-with-save
         (widen)
         (goto-char (point-min))
         (setq remaining-lines (forward-line (1- (car ws-butler-presave-coord)))))
        (unless (eq remaining-lines 0)
          (insert (make-string remaining-lines ?\n))))
      (el-patch-add
        (when (caddr ws-butler-presave-coord)
          (move-to-column (1+ (cadr ws-butler-presave-coord)) t)))
      (move-to-column (cadr ws-butler-presave-coord) t)
      (set-buffer-modified-p nil))))

;;;; doom
(add-hook! 'el-patch-pre-validate-hook (doom-require 'doom-lib 'projects))
(el-patch-defun doom/add-directory-as-project (dir)
  (el-patch-concat
   "Register an arbitrary directory as a project.\n\nUnlike `projectile-add-known-project', if DIR isn't a valid project, a .project\nfile will be created within it so that it will always be treated as one."
   (el-patch-swap
     " This\ncommand will throw an error if a parent of DIR is a valid project (which would\nmask DIR)."
     ""))
  (interactive "D")
  (when-let* ((proj-dir (doom-project-root dir)))
    (if (file-equal-p proj-dir dir)
        (user-error "ERROR: Directory is already a project: %s" proj-dir)
      (user-error "ERROR: Directory is already inside another project: %s" proj-dir)))
  (let ((short-dir (abbreviate-file-name dir)))
    (when (projectile-ignored-project-p dir)
      (user-error "ERROR: Directory is in projectile's ignore list: %s" short-dir))
    (el-patch-remove
      (dolist (proj projectile-known-projects)
        (when (file-in-directory-p proj dir)
          (user-error "ERROR: Directory contains a known project: %s" short-dir))
        (when (file-equal-p proj dir)
          (user-error "ERROR: Directory is already a known project: %s" short-dir))))
    (with-temp-file (doom-path dir ".project"))
    (message "Added directory as a project: %s" short-dir)
    (projectile-add-known-project dir)))

(add-hook! 'el-patch-pre-validate-hook (doom-require 'doom-lib 'buffers))
(el-patch-defun doom-visible-buffers (&optional buffer-list all-frames)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers
         (delete-dups
          (cl-loop for frame in (if all-frames (visible-frame-list) (list (selected-frame)))
                   ;; https://stackoverflow.com/a/21153375
                   (el-patch-add unless (string-equal "initial_terminal" (terminal-name frame)))
                   if (window-list frame)
                   nconc (mapcar #'window-buffer it)))))
    (if buffer-list
        (cl-loop for buf in buffers
                 unless (memq buf buffer-list)
                 collect buffers)
      buffers)))

;;;; execute-extended-command-for-buffer performance

;; Doing this does seem to leave out some commands, since setting the third
;; argument of `where-is-internal' to t leaves out menu-bar commands. But it's
;; unbearably slow if I don't do this.

(el-patch-feature simple)

;; The function returned by command-completion--command-for-this-buffer-function is what's slow
(el-patch-defun command-completion--command-for-this-buffer-function ()
  (let ((el-patch-add (gc-cons-threshold most-positive-fixnum)
                      (gc-cons-percentage 1.0))
        (keymaps
         ;; The major mode's keymap and any active minor modes.
         (nconc
          (and (current-local-map) (list (current-local-map)))
          (mapcar
           #'cdr
           (seq-filter
            (lambda (elem)
              (symbol-value (car elem)))
            minor-mode-map-alist)))))
    (lambda (symbol buffer)
      (or (command-completion-using-modes-p symbol buffer)
          ;; Include commands that are bound in a keymap in the
          ;; current buffer.
          (and (where-is-internal symbol keymaps (el-patch-add t))
               ;; But not if they have a command predicate that
               ;; says that they shouldn't.  (This is the case
               ;; for `ignore' and `undefined' and similar
               ;; commands commonly found in keymaps.)
               (or (null (get symbol 'completion-predicate))
                   (funcall (get symbol 'completion-predicate)
                            symbol buffer)))))))

(el-patch-defun command-completion-using-modes-and-keymaps-p (symbol buffer)
  "Return non-nil if SYMBOL is marked for BUFFER's mode or bound in its keymaps."
  (with-current-buffer buffer
    (let ((keymaps
           ;; The major mode's keymap and any active minor modes.
           (nconc
            (and (current-local-map) (list (current-local-map)))
            (mapcar
             #'cdr
             (seq-filter
              (lambda (elem)
                (symbol-value (car elem)))
              minor-mode-map-alist)))))
      (or (command-completion-using-modes-p symbol buffer)
          ;; Include commands that are bound in a keymap in the
          ;; current buffer.
          (and (where-is-internal symbol keymaps (el-patch-add t))
               ;; But not if they have a command predicate that
               ;; says that they shouldn't.  (This is the case
               ;; for `ignore' and `undefined' and similar
               ;; commands commonly found in keymaps.)
               (or (null (get symbol 'completion-predicate))
                   (funcall (get symbol 'completion-predicate)
                            symbol buffer)))
          ;; Include customize-* commands (do we need a list of such
          ;; "always available" commands? customizable?)
          (string-match-p "customize-" (symbol-name symbol))))))

;;;; diff-hl

(el-patch-feature diff-hl)
(el-patch-defun diff-hl-stage-some (&optional beg end)
  "Stage some or all of the current changes, interactively.
Pops up a diff buffer that can be edited to choose the changes to stage."
  (interactive "r")
  (diff-hl--ensure-staging-supported)
  (let* ((line-beg (and beg (line-number-at-pos beg t)))
         (line-end (and end (line-number-at-pos end t)))
         (file buffer-file-name)
         (dest-buffer (get-buffer-create "*diff-hl-stage-some*"))
         (orig-buffer (current-buffer))
         (diff-hl-update-async nil)
         ;; FIXME: If the file name has double quotes, these need to be quoted.
         (file-base (file-name-nondirectory file)))
    (with-current-buffer dest-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (diff-hl-diff-buffer-with-reference file dest-buffer nil 3)
    (with-current-buffer dest-buffer
      (let ((inhibit-read-only t))
        (when end
          (with-no-warnings
            (let (diff-auto-refine-mode)
              (diff-hl-diff-skip-to line-end)
              (diff-hl-split-away-changes 3)
              (el-patch-add
                (delete-region (point)
                               (save-excursion (forward-line 1) (point))))
              (diff-end-of-hunk)))
          (delete-region (point) (point-max)))
        (if beg
            (with-no-warnings
              (let (diff-auto-refine-mode)
                (diff-hl-diff-skip-to line-beg)
                (diff-hl-split-away-changes 3)
                (el-patch-swap
                  (diff-beginning-of-hunk)
                  (diff-end-of-hunk))))
          (goto-char (point-min))
          (forward-line 3))
        (delete-region (point-min) (point))
        ;; diff-no-select creates a very ugly header; Git rejects it
        (insert (format "diff a/%s b/%s\n" file-base file-base))
        (insert (format "--- a/%s\n" file-base))
        (insert (format "+++ b/%s\n" file-base)))
      (let ((diff-default-read-only t))
        (diff-hl-stage-diff-mode))
      (setq-local diff-hl-stage--orig orig-buffer))
    (pop-to-buffer dest-buffer)
    (message "Press %s and %s to navigate, %s to split, %s to kill hunk, %s to undo, and %s to stage the diff after editing"
             (substitute-command-keys "\\`n'")
             (substitute-command-keys "\\`p'")
             (substitute-command-keys "\\[diff-split-hunk]")
             (substitute-command-keys "\\[diff-hunk-kill]")
             (substitute-command-keys "\\[diff-undo]")
             (substitute-command-keys "\\[diff-hl-stage-finish]"))))

;; https://reddit.com/r/emacs/comments/1b6tb7d/committing_a_partial_changeset_in_emacs/ktln6vm/
(define-advice vc-git-command (:filter-args (args) akn/fix-vc-git-checkin-a)
  (pcase args
    (`(,_ 0 ,_ "apply" "--cached")
     (append args '("--unidiff-zero")))
    (_ args)))

;;;; tab-bar

(el-patch-feature tab-bar)
(el-patch-defun tab-bar-mouse-context-menu (event &optional posn)
  "Pop up the context menu for the tab on which you click.
EVENT is a mouse or touch screen event.  POSN is nil or the
position of EVENT."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (or posn (event-start event))))
         (tab-number (tab-bar--key-to-number (nth 0 item)))
         (menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))

    (cond
     ((eq tab-number t)
      (define-key-after menu [new-tab] '(menu-item "New tab" tab-bar-new-tab :help "Create a new tab"))
      (el-patch-add
        (when (modulep! :ui tab-bar)
          (define-key-after menu [new-tab] '(menu-item "New tab" +workspace/new :help "Create a new tab"))
          ;; TODO: add the list of workspace files to the context menu
          (define-key-after menu [load-workspace] '(menu-item "Load workspace from file" +workspace/load :help "Load a workspace from a file"))
          (define-key-after menu [resume-activity] '(menu-item "Resume activity" activities-resume :help "Resume some saved/suspended activity"))
          (define-key-after menu [new-activity] '(menu-item "New empty activity" activities-new :help "Open a newly defined activity in a new tab"))))
      (when tab-bar-closed-tabs
        (define-key-after menu [undo-close] '(menu-item "Reopen closed tab" tab-bar-undo-close-tab :help "Undo closing the tab"))))
     (t
      (define-key-after menu [duplicate-tab] `(menu-item "Duplicate" (lambda () (interactive) (tab-bar-duplicate-tab nil ,tab-number)) :help "Clone the tab"))
      (define-key-after menu [detach-tab] `(menu-item "Detach" (lambda () (interactive) (tab-bar-detach-tab ,tab-number)) :help "Move the tab to new frame"))
      (define-key-after menu [close] `(menu-item "Close" (lambda () (interactive) (tab-bar-close-tab ,tab-number)) :help "Close the tab"))
      (define-key-after menu [close-other] `(menu-item "Close other tabs" (lambda () (interactive) (tab-bar-close-other-tabs ,tab-number)) :help "Close all other tabs"))
      (el-patch-add
        (define-key-after menu [kill-buffers-close] `(menu-item "Kill buffers and close" (lambda () (interactive) (tab-bar-select-tab ,tab-number) (tabspaces-kill-buffers-close-workspace))
                                                      :help "Kill the workspace's buffers, then close the tab/workspace"))
        (define-key-after menu [rename] `(menu-item "Rename" (lambda () (interactive) (setq current-prefix-arg ,tab-number) (call-interactively #'tab-bar-rename-tab)) :help "Rename the tab"))
        (when (modulep! :ui tab-bar)
          (define-key-after menu [save] `(menu-item "Save workspace" (lambda () (interactive) (call-interactively (+workspace-save ,tab-number))) :help "Save the tab's workspace to a file"))
          (define-key-after menu [define-activity] `(menu-item "Define activity" (lambda () (interactive) (tab-bar-select-tab ,tab-number) (call-interactively #'activities-define)) :help "Define tab's state as a new activity"))
          (define-key-after menu [update-activity-default] `(menu-item "Redefine activity" (lambda () (interactive) (tab-bar-select-tab ,tab-number) (setq current-prefix-arg '(4)) (call-interactively #'activities-define)) :help "Update an existing activity's default state to this tab's state"))
          (when (and (bound-and-true-p activities-name-prefix) (string-prefix-p activities-name-prefix (+workspace--name (1- (or tab-number (+workspace--current-tab-number))))))
            (define-key-after menu [revert-activity] `(menu-item "Revert activity to default state" (lambda () (interactive) (tab-bar-select-tab ,tab-number) (activities-revert (activities-current))) :help "Revert the tab's activity to its default state"))
            (define-key-after menu [kill-activity] `(menu-item "Kill activity" (lambda () (interactive) (tab-bar-select-tab ,tab-number) (activities-kill (activities-current))) :help "Kill the tab's activity, discarding its current state"))
            (define-key-after menu [suspend-activity] `(menu-item "Suspend activity" (lambda () (interactive) (tab-bar-select-tab ,tab-number) (activities-suspend (activities-current))) :help "Suspend the tab's activity, saving its current state")))))))

    (popup-menu menu event)))

;;;; projectile

(use-package! projectile
  :config/el-patch
  ;; https://github.com/bbatsov/projectile/issues/1836
  ;; https://github.com/fuyu0425/projectile/commit/664365f77f3e71a17291891cf5f06104b839cffd
  (el-patch-defun projectile-project-root (&optional dir)
    "Retrieves the root directory of a project if available.
If DIR is not supplied its set to the current directory by default."
    (let ((dir (or dir default-directory)))
      ;; Back out of any archives, the project will live on the outside and
      ;; searching them is slow.
      (when (and (fboundp 'tramp-archive-file-name-archive)
                 (tramp-archive-file-name-p dir))
        (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
      ;; the cached value will be 'none in the case of no project root (this is to
      ;; ensure it is not reevaluated each time when not inside a project) so use
      ;; cl-subst to replace this 'none value with nil so a nil value is used
      ;; instead
      (cl-subst nil 'none
        (or
         ;; if we've already failed to find a project dir for this
         ;; dir, and cached that failure, don't recompute
         (let* ((cache-key (format "projectilerootless-%s" dir))
                (cache-value (gethash cache-key projectile-project-root-cache)))
           cache-value)
         ;; if the file isn't local, and we're not connected, don't try to
         ;; find a root now now, but don't cache failure, as we might
         ;; re-connect.  The `is-local' and `is-connected' variables are
         ;; used to fix the behavior where Emacs hangs because of
         ;; Projectile when you open a file over TRAMP. It basically
         ;; prevents Projectile from trying to find information about
         ;; files for which it's not possible to get that information
         ;; right now.
         (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
               (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
           (unless (or is-local is-connected)
             'none))
         ;; if the file is local or we're connected to it via TRAMP, run
         ;; through the project root functions until we find a project dir
         (el-patch-swap
           (cl-some
            (lambda (func)
              (let* ((cache-key (format "%s-%s" func dir))
                     (cache-value (gethash cache-key projectile-project-root-cache)))
                (if (and cache-value (file-exists-p cache-value))
                    cache-value
                  (let ((value (funcall func (file-truename dir))))
                    (puthash cache-key value projectile-project-root-cache)
                    value))))
            projectile-project-root-functions)
           (if-let* ((found (gethash (format "%s-%s" "projectile-root-any" dir) projectile-project-root-cache)))
               found
             (let ((cache-truename))
               (cl-some
                (lambda (func)
                  (let* ((cache-key (format "%s-%s" func dir))
                         (cache-value (gethash cache-key projectile-project-root-cache)))
                    (if (and cache-value (file-exists-p cache-value))
                        cache-value
                      (unless cache-truename (setq cache-truename (file-truename dir)))
                      (let ((value (funcall func cache-truename)))
                        (puthash cache-key value projectile-project-root-cache)
                        (when value (puthash (format "%s-%s" "projectile-root-any" dir)
                                             value projectile-project-root-cache))
                        value))))
                projectile-project-root-functions))))
         ;; if we get here, we have failed to find a root by all
         ;; conventional means, and we assume the failure isn't transient
         ;; / network related, so cache the failure
         (let ((cache-key (format "projectilerootless-%s" dir)))
           (puthash cache-key 'none projectile-project-root-cache)
           'none))))))

;;;; treesit

(when (and (modulep! :tools tree-sitter)
           (fboundp 'treesit-available-p))
  (add-hook! 'el-patch-pre-validate-hook
    (require 'treesit)
    (when (and (not (fboundp 'treesit-ensure-installed))
               (version< emacs-version "31.1"))
      (load! "modules/tools/tree-sitter/autoload/compat-30" doom-emacs-dir)))

  (el-patch-defun treesit-ensure-installed (lang)
    "Ensure that the grammar library for the language LANG is installed.
The option `treesit-auto-install-grammar' defines whether to install
the grammar library if it's unavailable."
    (or (treesit-ready-p lang t)
        (when (el-patch-wrap 2 0
                (and
                 ;; (not non-essential)
                 (not (when non-essential (message "not ensuring treesit grammar because non-essential") non-essential))
                 (or (eq treesit-auto-install-grammar 'always)
                     (and (eq treesit-auto-install-grammar 'ask)
                          (y-or-n-p (format "Tree-sitter grammar for `%s' is missing; install it?"
                                            lang))))))
          (treesit-install-language-grammar lang)
          ;; Check that the grammar was installed successfully
          (treesit-ready-p lang)))))

;;; validating
(cond
 (noninteractive nil)
 (load-in-progress
  ;; If we're loading this file, don't block startup
  (akn/after-idle! ((* 60 8) :timer-name akn/el-patch-timer)
    (el-patch-validate-all)))
 (t
  ;; If we're interactively using `eval-buffer', validate immediately
  (el-patch-validate-all)))
