;;; +vc.el -*- lexical-binding: t; -*-

;; TODO: https://github.com/redguardtoo/vc-msg (port of vim's git-messenger)
;; make SPC g o b -> open blame link
;; make SPC g m -> vc-msg
;; s-i

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

;;; key bindings

(map! :leader
      (:prefix "g"
       ;; NOTE: there's also SPC g / = magit-dispatch
       ;; name is misleading for git, it's restore/discard, not revert
       ;; (non-evil binding = k)
       :desc "Discard hunk" "k" #'+vc-gutter/save-and-revert-hunk
       :desc "Discard file" "K" #'vc-revert
       :desc "Discard hunk" "x" #'+vc-gutter/save-and-revert-hunk
       :desc "Discard file" "X" #'vc-revert
       "r" #'ignore
       "R" #'ignore

       ;; previously just `magit-blame-addition'
       "B"   (akn/cmds! (bound-and-true-p magit-blame-mode) #'magit-blame-quit #'magit-blame-addition)

       :desc "Magit run"          "!" #'magit-run
       :desc "Magit status here"  "RET" #'magit-status-here
       :desc "Magit git command"  "Q" #'magit-git-command
       ;; TODO: implement "unstage hunk"
       :desc "Magit stash"        "Z" #'magit-stash
       :desc "Magit quit"         "q" #'akn/magit-quit
       :desc "Magit push"         "p" #'magit-push
       :desc "Refresh"            "r" #'akn/vc-full-refresh
       ;; "g c" #'magit-commit
       ;; previously magit-branch-checkout
       :desc "Magit branch"       "b" #'magit-branch
       :desc "Magit ignore"       "i" #'magit-gitignore
       :desc "Magit reset"        "O" #'magit-reset
       :desc "Magit worktree"     "%" #'magit-worktree

       :desc "unstage hunk at point" "u" #'akn/unstage-hunk
       :desc "stage hunk at point"   "s" #'akn/stage-hunk

       :desc "Log current"        "l l" #'magit-log-current
       :desc "Log..."             "l L" #'magit-log-current
       :desc "Log..."             "l /" #'magit-log-current
       :desc "Log HEAD"           "l h" #'magit-log-head
       :desc "Log related"        "l u" #'magit-log-related
       :desc "Log other"          "l o" #'magit-log-other
       :desc "Log all branches"   "l b" #'magit-log-all-branches
       :desc "Log all references" "l a" #'magit-log-all
       :desc "Reflog current"     "l R" #'magit-reflog-current
       :desc "Reflog HEAD"        "l H" #'magit-reflog-head
       :desc "Reflog other"       "l O" #'magit-reflog-other

       :desc "Magit amend"          "c a" #'magit-commit-amend
       :desc "Magit extend"         "c e" #'magit-commit-extend
       :desc "Magit commit..."      "c C" #'magit-commit
       :desc "Magit commit..."      "c /" #'magit-commit
       :desc "Magit alter"          "c A" #'magit-commit-alter
       :desc "Magit augment"        "c n" #'magit-commit-augment
       :desc "Magit fixup"          "c F" #'magit-commit-fixup
       :desc "Magit instant fixup"  "c F" #'magit-commit-instant-fixup
       :desc "Magit squash"         "c s" #'magit-commit-squash
       :desc "Magit instant squash" "c S" #'magit-commit-instant-squash
       :desc "Magit reword"         "c w" #'magit-commit-reword
       :desc "Magit revise"         "c W" #'magit-commit-revise
       :desc "git undo-last-commit" "c u"   #'akn/git-undo-last-commit
       :desc "git redo-commit"      "c C-r" #'akn/git-redo-commit

       :desc "Open gitup" "o u" (akn/defun akn/open-gitup ()
                                  (interactive)
                                  (call-process "gitup")))


      ;; previously toggled evil-goggles, but I always want that on
      "t g" nil
      (:prefix ("t g" . "git")
               "b" #'blamer-mode
               "C-b" #'global-blamer-mode
               "B" (akn/cmds! (bound-and-true-p magit-blame-mode) #'magit-blame-quit #'magit-blame-addition)
               "t" #'git-timemachine-toggle))

(after! which-key
  (akn/remove-from-list 'which-key-replacement-alist '(("\\`M-SPC t g\\'") nil . "Evil goggles"))
  (akn/remove-from-list 'which-key-replacement-alist '(("\\`SPC t g\\'") nil . "Evil goggles"))
  (akn/remove-from-list 'which-key-replacement-alist '(("\\`M-SPC g r\\'") nil . "Revert hunk at point"))
  (akn/remove-from-list 'which-key-replacement-alist '(("\\`SPC g r\\'") nil . "Revert hunk at point"))
  (akn/remove-from-list 'which-key-replacement-alist '(("\\`M-SPC g R\\'") nil . "Revert file"))
  (akn/remove-from-list 'which-key-replacement-alist '(("\\`SPC g R\\'") nil . "Revert file")))

;;; file-based stuff (diff-hl)

(after! diff-hl
  (setq! diff-hl-update-async nil))

(defun akn/vc-full-refresh ()
  (interactive)
  (let ((inhibit-message-regexps (cons (rx "No buffers need saving") inhibit-message-regexps)))
    (call-interactively #'projectile-save-project-buffers))
  (when (fboundp 'magit-refresh) (call-interactively #'magit-refresh))
  (when (fboundp 'vc-refresh-state) (call-interactively #'vc-refresh-state))
  (when (fboundp 'diff-hl-update) (diff-hl-update))
  (when (fboundp 'git-gutter:update-all-windows) (git-gutter:update-all-windows))
  (shell-command "git status -sbu --ignore-submodules=untracked"))

(defun akn/undo-disabled-p ()
  ;; See definition of `buffer-disable-undo'
  (eq buffer-undo-list t))
(define-advice +vc-gutter/save-and-revert-hunk (:override (ask-before-revert-hunk) akn/a)
  "Invoke `diff-hl-revert-hunk' with `vc-suppress-confirm' set.

If a prefix argument is provided, ask before reverting hunk."
  (interactive "P")
  (akn/letf! ((diff-hl-ask-before-revert-hunk (or (akn/undo-disabled-p)
                                                  ask-before-revert-hunk)))
    (+vc-gutter/revert-hunk t)))

(defvar akn/after-stage-hook nil)
(defun akn/stage-hunk ()
  (interactive)
  (if (fboundp 'diff-hl-stage-dwim)
      (diff-hl-stage-dwim)
    (+vc-gutter/stage-hunk))
  (run-hooks 'akn/after-stage-hook))

(defvar akn/after-unstage-hook nil)
(defun akn/unstage-hunk ()
  (interactive)
  (save-window-excursion
    (call-interactively #'magit-status-here)
    (magit-unstage))
  (run-hooks 'akn/after-unstage-hook))

;;; magit

(defun akn/git-undo-last-commit (&optional args)
  (interactive (progn (require 'magit-commit) (magit-commit-arguments)))
  (require 'magit-process)
  (magit-run-git-with-editor "undo-last-commit" args))
(defun akn/git-redo-commit (&optional args)
  (interactive (progn (require 'magit-commit) (magit-commit-arguments)))
  (require 'magit-process)
  (magit-run-git-with-editor "redo-commit" args))

(use-package! magit
  :init
  (setq magit-define-global-key-bindings 'recommended)
  :config
  (map! :map magit-mode-map
        [remap +fold/outline-cycle-all-simple] (akn/cmds! (eq last-command #'akn/magit-close-all-sections)
                                                          #'akn/magit-open-all-sections
                                                          #'akn/magit-close-all-sections)
        [remap +fold/outline-cycle-all] #'akn/magit-toggle-all-sections)
  (defun akn/magit-open-all-sections ()
    (interactive)
    (magit-map-sections #'magit-section-show))
  (defun akn/magit-close-all-sections ()
    (interactive)
    (ignore-errors
      (magit-map-sections #'magit-section-hide)))

  ;; if I try to commit and nothing is staged, then stage everything without asking
  (setq! magit-commit-ask-to-stage 'stage)
  (setq! git-commit-style-convention-checks (remove 'overlong-summary-line git-commit-style-convention-checks))

  ;; https://magit.vc/manual/magit/Wip-Modes.html
  (magit-wip-mode)

  ;; Doom disables this by default.
  ;; If it turns out to be too aggressive, we could also try
  ;; adding hooks that run #'magit-save-repository-buffers
  (setq! magit-save-repository-buffers 'dontask)

  (pushnew! magit-no-confirm
            'safe-with-wip
            'stage-all-changes
            'unstage-all-changes
            'untrack
            'rename)

  (pushnew! magit-published-branches
            "origin/master"
            "upstream/master"
            "origin/main"
            "upstream/main")

  (setq magit-repository-directories `(("~/prog" . 2)
                                       ("~/.config/emacs" . 0)
                                       ("~/org" . 0)
                                       ("~/Documents/obsidian-vault/" . 0))
        magit-repolist-columns '(("↓" 3 magit-repolist-column-unpulled-from-upstream
                                  ((:right-align t)
                                   (:sort <)))
                                 ("⇡" 3 magit-repolist-column-unpushed-to-upstream
                                  ((:right-align t)
                                   (:sort <)))
                                 ("*" 3 magit-repolist-column-stashes
                                  ((:right-align t)
                                   (:sort <)))
                                 ("!?+" 3 magit-repolist-column-flags nil)
                                 ("Path" 45 magit-repolist-column-path nil)
                                 ;; ("Name" 25 magit-repolist-column-ident nil)
                                 ("Version" 25 magit-repolist-column-version
                                  ((:sort magit-repolist-version<)))
                                 ("Branch" 20 magit-repolist-column-branch))
        magit-repolist-column-flag-alist '((magit-untracked-files . "?")
                                           (magit-unstaged-files . "!")
                                           (magit-staged-files . "+")))
  (akn/advise-letf! magit-list-repositories (akn/a)
    (directory-abbrev-alist (cons `(,(directory-abbrev-make-regexp (akn/expand-file "~/prog")) . "")
                                  directory-abbrev-alist)))

  (when (not (member "commit.verbose=false" magit-git-global-arguments))
    (cl-callf append magit-git-global-arguments '("-c" "commit.verbose=false")))

  (defadvice! akn/switch-to-magit-a (&rest _)
    "Switch to the magit window if it exists."
    :before #'magit-commit-create
    :before #'magit-commit-amend
    ;; :before #'magit-commit-extend
    :before #'magit-commit-reword
    :before #'magit-commit-squash
    :before #'magit-commit-augment
    :before #'magit-commit-instant-fixup
    (let* ((visible-windows (window-list))
           (repo (magit-toplevel))
           (visible-magit-windows (seq-filter #'(lambda (w)
                                                  (and (+magit-buffer-p (window-buffer w))
                                                       (equal (with-selected-window w (magit-toplevel))
                                                              repo)))
                                              visible-windows)))
      (when visible-magit-windows
        (select-window (car visible-magit-windows)))))

  (defun akn/magit-quit ()
    (interactive)
    (let* ((orig-window (get-buffer-window))
           (visible-windows (window-list))
           (visible-magit-windows (seq-filter #'(lambda (w) (+magit-buffer-p (window-buffer w)))
                                              visible-windows)))
      (dolist (window visible-magit-windows)
        (progn
          (select-window window)
          (+magit/quit)))
      (select-window orig-window)))

  (defun akn/inside-git-repo-p ()
    (magit-toplevel default-directory))
  (defun akn/current-magit-status-buffer ()
    (and (akn/inside-git-repo-p)
         (magit-get-mode-buffer 'magit-status-mode nil 'visible)))

  (add-hook! '(after-save-hook akn/after-stage-hook akn/after-unstage-hook) :depth 90
    (defun akn/magit-after-save-refresh-status ()
      (when (akn/current-magit-status-buffer)
        (magit-after-save-refresh-status))))

  ;; I want magit to open to the side, not replace the whole buffer
  (defadvice! akn/+magit-display-buffer-fn-a (_fn buffer)
    :around #'+magit-display-buffer-fn
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
                '(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))

               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode))))
                ;; CHANGED
                ;; '(display-buffer-same-window) ;;original
                ;; '(display-buffer-at-bottom)
                nil)

               ('(+magit--display-buffer-in-direction)))))))

(after! magit-commit
  (transient-append-suffix #'magit-commit "n"
    '("u" "Undo last" akn/git-undo-last-commit))
  (transient-append-suffix #'magit-commit "u"
    '("C-r" "Redo" akn/git-redo-commit)))

;;; magit-delta

(use-package! magit-delta
  :if (executable-find "delta")
  :defer-incrementally (magit xterm-color dash)
  :after-call magit-mode-hook
  ;; :ghook 'magit-mode-hook
  :commands akn/magit-delta-global-mode
  :init
  (map! :leader
        "t g d" (akn/cmds! (not (derived-mode-p 'magit-mode)) #'akn/magit-delta-global-mode
                           magit-delta-mode #'magit-delta-mode
                           #'akn/enable-magit-delta-mode)

        "t g D" #'akn/magit-delta-global-mode)
  :config
  (defvar akn--magit-delta-should-refresh nil)
  (add-hook! 'magit-delta-mode-hook
    (defun akn/magit-refresh-buffer-after-toggling-delta-explicitly-h ()
      (when (and (derived-mode-p 'magit) (or magit-delta-mode-set-explicitly (not magit-delta-mode) akn--magit-delta-should-refresh))
        (magit-refresh-buffer))))
  (defun akn/enable-magit-delta-mode ()
    (interactive)
    (unless magit-delta-mode
      (let ((akn--magit-delta-should-refresh t))
        (magit-delta-mode))))
  (define-globalized-minor-mode akn/magit-delta-global-mode magit-delta-mode magit-delta-mode ;akn/enable-magit-delta-mode
    :predicate '(magit-mode)
    :group 'akn))
  ;; (akn/magit-delta-global-mode))

;;; magit-todos

;; copied from doom
(use-package! magit-todos
  :after magit
  :defer-incrementally (cl-lib compile grep seq async dash f hl-todo s with-editor git-commit package eieio transient magit rx re-builder macroexp advice ring pcase pcre2el)
  :init
  (map! :desc "List project todos" :leader "p t" #'magit-todos-list)
  ;; (magit-todos-mode)
  :config
  (setq magit-todos-keyword-suffix
        (rx (? (any "([")
               (+ (not (any ")]")))
               (any ")]"))
            (? ":"))) ; make colon optional
  (define-key magit-todos-section-map "j" nil))

;;; forge

(after! forge
  (add-hook! 'forge-topic-list-mode-hook #'+word-wrap-mode))

;;; fugitive

(when (modulep! :editor evil)
  ;; inspired by https://github.com/tpope/vim-fugitive
  (evil-ex-define-cmd "git" #'akn/fugitive)
  (evil-ex-define-cmd "Git" #'akn/fugitive)

  (evil-define-command akn/fugitive (cmd)
    (interactive "<a>")
    (require 'with-editor)
    (pcase (and cmd (split-string-shell-command cmd))
      ((or `() `(""))
       (call-interactively #'magit-status))
      (`("status")
       (shell-command "git status -sbu --ignore-submodules=untracked"))
      (`("diff")
       (magit-diff-unstaged))
      (`("diff" ,(or "--staged" "--cached"))
       (magit-diff-staged))
      (`("blame")
       (call-interactively #'magit-blame-addition))
      (`("add")
       (call-interactively #'magit-file-stage))
      (`("commit" . ,_)
       (let ((buf (generate-new-buffer "*akn/git-output*")))
         (with-editor* "GIT_EDITOR"
           (akn/run-command (format "git --no-pager %s" cmd)
                           :output-buffer buf
                           :on-output (lambda (str) (message "%s" (string-trim str)))))))
      (_
       (let ((buf (akn/popup-new-buffer "*akn/git-output*" :size 0.35 :ttl 60 :select nil)))
         (with-current-buffer buf
           (view-mode))
         ;; (display-buffer buf)
         (with-editor* "GIT_EDITOR"
           (akn/run-command (format "git --no-pager -c color.ui=always %s" cmd)
                           :output-buffer buf)))))))

;;; vc

;; Doom sets this to '(SVN Git Hg)
(setq! vc-handled-backends '(Git))

;;; transient

(after! transient
  (setq! transient-default-level 7
         transient-highlight-higher-levels t))
(use-package! transient-showcase
  :commands (tsc-showcase akn/transient-showcase)
  :config
  (def-project-mode! akn/transient-showcase-mode
    :match (akn/files-regex '("~/.config/emacs/.local/straight/repos/transient-showcase"))
    :on-enter (message "Press C-c s for #'tsc-showcase"))
  (map! :map akn/transient-showcase-mode-map
        "C-c s" #'tsc-showcase)
  (defun akn/transient-showcase ()
    (interactive)
    (find-file "~/.config/emacs/.local/straight/repos/transient-showcase/transient-showcase.org")))

(map! :map transient-map
      [escape] #'transient-quit-one
      "@" #'transient-help
      "q" #'transient-quit-all)

;;; git-commit-mode

(use-package! git-commit
  :autoload git-commit-setup
  :init
  ;; https://github.com/magit/magit/issues/3931
  (let ((git-commit-filename-regexp
         (or (bound-and-true-p git-commit-filename-regexp)
             "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")))
    (add-to-list 'auto-mode-alist (cons git-commit-filename-regexp #'git-commit-setup)))
  (remove-hook 'doom-first-file-hook #'global-git-commit-mode))

;;; smerge
(use-package! smerge-mode
  :commands smerge-vc-next-conflict)

(map! "M-N" #'smerge-vc-next-conflict
      [remap smerge-next] #'smerge-vc-next-conflict
      :after smerge-mode
      :map smerge-basic-map
      "j" #'smerge-next
      "k" #'smerge-prev
      "N" #'smerge-vc-next-conflict
      :map smerge-mode-map
      "M-RET" #'smerge-keep-current
      "M-a" #'smerge-keep-all
      "M-p" #'smerge-prev
      "M-n" #'smerge-next
      "M-N" #'smerge-vc-next-conflict
      "M-e" #'smerge-ediff
      (:localleader
       "j" #'smerge-next
       "k" #'smerge-prev))

(add-hook! 'smerge-mode-hook
  (defun akn/smerge-hints-h ()
    (message "%s"
             (substitute-command-keys
              (if smerge-mode
                  "Merge conflicts: \\`M-n' = next, \\[smerge-prev] = previous, \\[smerge-keep-current] = choose version under cursor, \\[smerge-ediff] = ediff"
                (setq-local smerge-change-buffer-confirm nil)
                "smerge done in this file (\\[smerge-vc-next-conflict] = next conflict in other file)")))))

(defun akn/smerge-delete-trailing-whitespace ()
  (interactive)
  (require 'smerge-mode)
  (smerge-match-conflict)
  (delete-trailing-whitespace (match-beginning 0) (match-end 0)))
(defun akn/smerge-tabify ()
  (interactive)
  (require 'smerge-mode)
  (smerge-match-conflict)
  (tabify (match-beginning 0) (match-end 0)))
(defun akn/smerge-untabify ()
  (interactive)
  (require 'smerge-mode)
  (smerge-match-conflict)
  (untabify (match-beginning 0) (match-end 0)))

(defun akn/smerge-mediate ()
  "Like git-mediate."
  (interactive)
  (require 'smerge-mode)
  (smerge-match-conflict)
  (smerge-ensure-match 1)
  (smerge-ensure-match 3)
  (condition-case err
      (smerge-ensure-match 2)
    (error
     (user-error "%s" (concat (error-message-string err) "; a 3-way diff is needed, but this seems to be a 2-way diff"))))
  (if-let* ((upper (match-string-no-properties 1))
            (base (match-string-no-properties 2))
            (lower (match-string-no-properties 3)))
      (cond
       ((equal upper base)  (message "upper and base match; choosing lower") (smerge-keep-lower))
       ((equal lower base)  (message "lower and base match; choosing upper") (smerge-keep-upper))
       ((equal upper lower) (message "upper and lower match; choosing upper") (smerge-keep-upper))
       (t (message "Can't resolve this conflict")))
    (user-error "can't get all 3 parts of conflict")))

;;; file-local variables

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
