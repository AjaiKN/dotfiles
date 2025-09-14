;;; +general.el -*- lexical-binding: t; -*-

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

;;; Misc
(setq confirm-kill-emacs nil)

(setq-default
 ;;window-combination-resize t                     ;; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

;; previously 120
(setq kill-ring-max (max kill-ring-max 500))

(setq message-log-max (max message-log-max 8000))

;; Save system clipboard string into kill ring before replacing them
(setq save-interprogram-paste-before-kill t)

;; also try 'overlay and 'child-frame
(setq show-paren-context-when-offscreen 'overlay)

;; only works in insert state
(setq mouse-drag-and-drop-region t)

;; (setq! frame-title-format "%b")

(setq delete-by-moving-to-trash (not noninteractive) ; Doom only does this by default on Mac for some reason
      magit-delete-by-moving-to-trash t
      remote-file-name-inhibit-delete-by-moving-to-trash nil)

;;; modeline

;; light modeline and regular doom-modeline
(add-hook! '(+modeline-global-mode-hook doom-modeline-mode-hook)
  (defun akn/modeline-stuff ()
    (interactive)
    (setq! display-time-default-load-average nil)
    (display-time-mode 1)

    (setq! battery-mode-line-format "[%b%p%%, %t left]  ")
    (unless (string-match-p "^Power N/A" (shut-up (battery)))
      (display-battery-mode 1))))

;; for performance
(setq doom-modeline-workspace-name nil)

;;;; doom-modeline

(use-package! doom-modeline
  :defer t
  :when (modulep! :ui modeline -light)
  :init
  (setq! doom-modeline-buffer-encoding (if (featurep :system 'windows) t 'nondefault)
         doom-modeline-always-show-macro-register t
         ;; doom-modeline-minor-modes t
         doom-modeline-enable-word-count t)
  :config
  (pushnew! doom-modeline-continuous-word-count-modes
            'text-mode))

;;;; doom modeline +light

(when (modulep! :ui modeline +light)
  ;; TODO: I disabled showing the workspace in the frame title because
  ;; of a bug where after switching workspaces or windows, sometimes it
  ;; doesn't appear.

  ;; (def-modeline-var! akn/title-bar-workspace-cache nil)
  ;; (add-hook! '(change-major-mode-after-body-hook
  ;;              ;; In case the user saves the file to a new location
  ;;              after-save-hook
  ;;              ;; ...or makes external changes then returns to Emacs
  ;;              focus-in-hook
  ;;              ;; ...or when we change the current project!
  ;;              projectile-after-switch-project-hook
  ;;              ;; ...when the visited file changes (e.g. it's renamed)
  ;;              after-set-visited-file-name-hook
  ;;              ;; ...when the underlying file changes
  ;;              after-revert-hook)
  ;;            :append
  ;;            #'akn/delayed-update-title-bar-workspace)
  ;; (defun akn/update-title-bar-workspace (&rest _)
  ;;   (setq akn/title-bar-workspace-cache
  ;;         (let ((workspace (and (fboundp #'safe-persp-name)
  ;;                               (+workspace-current-name))))
  ;;           (unless (or (not workspace)
  ;;                       (and (bound-and-true-p +modeline--buffer-id-cache)
  ;;                            (string-prefix-p workspace +modeline--buffer-id-cache)))
  ;;             (format "%s: " workspace)))))
  ;; (defun akn/delayed-update-title-bar-workspace (&rest _)
  ;;   (run-with-timer 0 nil #'akn/update-title-bar-workspace))

  ;; (defun akn/+modeline--generate-buffer-id-cache-h (&rest _)
  ;;   (+modeline--generate-buffer-id-cache-h))
  ;; (advice-add #'persp-frame-switch  :after #'akn/delayed-update-title-bar-workspace)
  ;; (advice-add #'persp-window-switch :after #'akn/delayed-update-title-bar-workspace)
  ;; (advice-add #'persp-frame-switch  :after #'akn/+modeline--generate-buffer-id-cache-h)
  ;; (advice-add #'persp-window-switch :after #'akn/+modeline--generate-buffer-id-cache-h)

  (setq! frame-title-format
         '(""
           ;; akn/title-bar-workspace-cache
           ;; +modeline-buffer-identification
           ((:eval (propertize (or +modeline--buffer-id-cache "%b")
                               'face (cond ((buffer-modified-p) '(error bold mode-line-buffer-id))
                                           ((+modeline-active) 'mode-line-buffer-id))
                               'help-echo (or +modeline--buffer-id-cache (buffer-name)))))
           " (%+)")))

;;;; modeline word count

(use-package! org-wc
  :autoload akn/org-word-count-number
  :config
  (setq! org-wc-default-link-count 'description)
  ;; TODO: PR
  (defadvice! akn/org-wc/remove-cached-word-counts-a (beg end &rest _)
    :before #'org-word-count
    (remove-text-properties beg end '(:org-wc t)))
  (defun akn/org-word-count-number (_beg end)
    "The regular `org-word-count' doesn't return the number of words,
it just displays it."
    (akn/org-wc/remove-cached-word-counts-a beg end)
    (org-word-count-aux beg end)))
(when (modulep! :ui modeline +light)
  (def-modeline-var! akn/modeline-word-count ""
                     "The string for the word count in the modeline."
                     :local t)
  (defun akn/modeline-word-count-update (&optional buffer)
    (when (or (null buffer) (buffer-live-p buffer))
      (with-current-buffer (or buffer (current-buffer))
        (setq-local akn/modeline-word-count
                    (format " %s words "
                            (apply (if (derived-mode-p 'org-mode)
                                       #'akn/org-word-count-number
                                     #'count-words)
                                   (if (use-region-p)
                                       (list (region-beginning) (region-end))
                                     (list (point-min) (point-max)))))))))
  (defun akn/modeline-word-count-update-after-idle (&rest _)
    (let ((buffer (current-buffer)))
      (akn/after-idle! (akn/modeline-word-count-idle-time
                        :timer-name akn/modeline-word-count-idle-timer
                        :starting-now t)
        (akn/modeline-word-count-update buffer))))
  (akn/defvar-setq akn/modeline-word-count-idle-time 0.3)
  (defvar-local akn/modeline-word-count-idle-timer nil)
  (define-minor-mode akn/modeline-word-count-mode
    "Show word count in the modeline."
    :global nil
    :group 'akn
    (if akn/modeline-word-count-mode
        (akn/modeline-word-count-update)
      (progn
        (when akn/modeline-word-count-idle-timer
          (cancel-timer akn/modeline-word-count-idle-timer))
        (setq akn/modeline-word-count "")))
    (akn/mode-add-hook! akn/modeline-word-count-mode 'post-command-hook :local
      #'akn/modeline-word-count-update-after-idle))
  (add-to-list 'mode-line-misc-info '("" akn/modeline-word-count))
  (after! so-long (add-to-list 'so-long-minor-modes 'akn/modeline-word-count-mode)))

;;; recentf

(setq! recentf-case-fold-search t)

(defun akn/keep-predicate (file)
  "Faster version of `recentf-keep-default-predicate'.

This is used to replace both `recentf-keep-default-predicate' (by
changing the value of `recentf-keep') and
`projectile-keep-project-p', since their original implementations
are exactly the same too."
  (condition-case err
      (let
          ((is-maybe-remote (akn/file-remote-p file)))
        (cond
         ((and is-maybe-remote (file-remote-p file nil t))
          (file-readable-p file))
         ((and is-maybe-remote (file-remote-p file)))
         ((file-readable-p file))))
    (error
     (message "akn/keep-predicate error: %S" err)
     t)))
(advice-add #'projectile-keep-project-p :override #'akn/keep-predicate)

(after! recentf
  (setq recentf-keep (remq #'recentf-keep-default-predicate recentf-keep))
  (add-to-list 'recentf-keep #'akn/keep-predicate)

  (add-to-list 'recentf-exclude (rx ".local/etc/workspaces/autosave" eos))
  (add-to-list 'recentf-exclude (rx "/elgrep-data.el" eos))
  ;; originally 20, Doom set to 200
  (setq recentf-max-saved-items (max 5000 recentf-max-saved-items))

  ;; similar to https://news.ycombinator.com/item?id=33186412
  (setq recentf-save-file (concat doom-cache-dir "my_recentf"))

  ;; Doom wants to do this on emacs quit, but I don't.
  (remove-hook 'kill-emacs-hook #'recentf-cleanup)
  (setq! recentf-auto-cleanup (* 22 60))

  ;; timer for autosaving recentf
  (akn/after-idle! (63 :each-idle t :timer-name akn/recentf-timer)
    (when (and recentf-mode (recentf-enabled-p))
      (recentf-save-list))))

;;; savehist

(after! savehist
  (akn/after-idle! (95 :each-idle t :timer-name akn/savehist-timer)
    (savehist-autosave))

  ;; similar to https://news.ycombinator.com/item?id=33186412
  (setq savehist-file (concat doom-cache-dir "my_savehist")))

;;; input methods
(defadvice! akn/say-input-method-a (&rest _)
  :after #'set-input-method
  :after #'toggle-input-method
  (message "input method: %s" (if (modulep! :editor evil) evil-input-method current-input-method)))

(use-package! devanagari-qwerty
  :after-call doom-first-input-hook
  :defer-incrementally (quail)
  :init
  (when (modulep! :input reverse-im)
    (after! reverse-im
      (add-to-list '+reverse-im-input-source-alist '("com.apple.keylayout.Devanagari-QWERTY" . "devanagari-qwerty"))
      (require 'devanagari-qwerty)))
  :config
  (setq! default-input-method "devanagari-qwerty"))

;; (setq! default-input-method "devanagari-itrans")
;; (setq! default-input-method "devanagari-kyoto-harvard")

;;; which-key

(after! which-key
  ;; https://tecosaur.github.io/emacs-config/config.html#which-key
  (setq which-key-idle-delay 0.4 ;default 1
        which-key-idle-secondary-delay 0.05
        which-key-show-transient-maps t
        which-key-allow-evil-operators (modulep! :editor evil)
        ;; seems to mess stuff up
        which-key-show-operator-state-maps nil)

  (which-key-add-key-based-replacements "C-x 4" "window")
  (which-key-add-key-based-replacements "C-x 5" "frame")
  (which-key-add-key-based-replacements "C-x 6" "2-column")
  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x r" "reg/bkmrk/rect/undo-fu-session")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x v" "vc")
  (which-key-add-key-based-replacements "C-x w" "window")
  (which-key-add-key-based-replacements "C-x w ^" "move window to frame/tab")
  (which-key-add-key-based-replacements "C-x v" "vc")
  (which-key-add-key-based-replacements "C-x x" "buffer")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")
  (which-key-add-key-based-replacements "C-x M-c" "xkcd")
  (which-key-add-key-based-replacements "C-x RET" "encodings/input")

  ;; "I also think that having evil- appear in so many popups is a bit too verbose, let’s change that, and do a few other similar tweaks while we’re at it."
  (setq which-key-allow-multiple-replacements t)
  (pushnew!
   which-key-replacement-alist
   `((""              . ,(rx bos (? "+") "evil" (? (any ":-")) (? "a-") (group (* nonl)))) . (nil . "-\\1"))
   `((""              . ,(rx "-and-"))                                                     . (nil . "-&-"))
   `((,(rx bos "g z") . ,(rx bos "+multiple-cursors/evil" (group (* nonl))))               . (nil . "+\\1"))
   `((,(rx bos "g z") . ,(rx "make"))                                                      . (nil . "mk"))
   `((,(rx bos "g z") . ,(rx "move"))                                                      . (nil . "mv"))
   `((,(rx bos "g s") . ,(rx bos "evilem-" (? "-") "motion-" (group (* nonl))))            . (nil . "-\\1")))
  ;; '(("\\`SPC w" . "\\`\\(.*\\)-?-?window-?-?\\(.*\\)") . (nil . "\\1\\2"))))
  ;; (setq!)
  ;; (setq which-key-replacement-alist (cdddr which-key-replacement-alist))
  ;; (setq! which-key-unicode-correction 3)
  ;; added a zero-width space (https://en.wikipedia.org/wiki/Zero-width_space) to correct for it
  ;; (setq! which-key-dont-use-unicode t)
  ;; (setq! which-key-ellipsis "..")
  (setq! which-key-ellipsis "…"))

;;;; which-key bug fix

;; e.g., when pressing "z" in normal mode, I was getting this error:
;;     Error running timer ‘which-key--update’: (void-function +spell/add-word)
(akn/advise-letf! which-key--propertize-description (akn/bugfix-a)
  (advice-add #'documentation :around #'akn/ignore-errors-a))

;;; bell
(setq ring-bell-function #'doom-themes-visual-bell-fn
      evil-search-wrap-ring-bell t
      show-paren-ring-bell-on-mismatch nil)

;;; persp-mode bug?

;; Problems when quitting a terminal frame.
;; The problem seems to have something to do with the fact
;; that according to the `delete-frame-functions' docs,
;; these functions might be called after the frame is already
;; dead. This seems to cause 2 problems:
;; 1. `persp-delete-frame', which is part of `delete-frame-functions', throws an error.
;; 2. `+workspaces-delete-associated-workspace-h', which is also part of `delete-frame-functions',
;;    doesn't throw an error, but it fails to delete the associated workspace like it's supposed to.

;; Problem 1:
(defun akn/wrapper-persp-delete-frame (&rest args)
  (shut-up
    (with-demoted-errors "persp-delete-frame: %s"
      (when (bound-and-true-p persp-mode)
        (apply #'persp-delete-frame args)))))
(add-hook 'persp-mode-hook #'akn/replace-persp-delete-frame-with-wrapper-h)
(defun akn/replace-persp-delete-frame-with-wrapper-h (&rest _)
  (when (boundp 'delete-frame-functions)
    (setq delete-frame-functions
          (cl-loop for f in delete-frame-functions
                   collect (if (eq f #'persp-delete-frame) #'akn/wrapper-persp-delete-frame f)))))
(akn/replace-persp-delete-frame-with-wrapper-h)
(defadvice! akn/winner-save-old-configurations-a (fn &rest args)
  :around #'winner-save-old-configurations
  (shut-up
    (with-demoted-errors "winner-save-old-configurations: %s"
      (apply fn args))))

;; Problem 2:
;; This is the only real solution I could find to make the
;; workspace correctly get deleted when quitting: run
;; `+workspaces-delete-associated-workspace-h' *beforehand*
;; instead of relying on it during `delete-frame-functions.'
(defadvice! akn/quit-thing-a (&optional frame &rest _)
  :before #'delete-frame
  (with-demoted-errors "akn/quit-thing-a: %s"
    (when (fboundp '+workspaces-delete-associated-workspace-h)
      (+workspaces-delete-associated-workspace-h frame))))

;; (remove-hook! 'delete-frame-functions #'akn/wrapper-persp-delete-frame)

;; (defadvice! akn/persp-delete-frame-a (fn &rest args)
;;   :around #'persp-delete-frame
;;   :around #'persp--deactivate
;;   (with-demoted-errors "persp-delete-frame: %s"
;;     (apply fn args)))
;; (akn/advise-letf! persp-delete-frame (akn/a2)
;;   (defadvice! akn/persp-delete-frame-a2-select-frame-a (fn frame &rest args)
;;     :around #'select-frame
;;     (if (frame-live-p)
;;         (apply fn frame args)
;;       (message "frame dead :("))))

;;; commenting bug in insert state (PR to doom?)
(map! :nvieomrg "s-/"
      (akn/defun akn/evilnc-comment-or-uncomment-lines (&rest args)
        (interactive "p")
        (let ((was-at-eol (string-match-p (rx bos (* whitespace) eos) (buffer-substring (point) (pos-eol)))))
          (akn/evil-save-state
            (save-mark-and-excursion
              (if (fboundp #'evilnc-comment-or-uncomment-lines)
                  (apply #'evilnc-comment-or-uncomment-lines args)
                (comment-line 1))))
          (when was-at-eol (end-of-line)) ;e.g. when typing s-/ on an empty line
          (deactivate-mark))))

;;; smartparens

(use-package! smartparens
  :defer-incrementally t
  :config
  ;; https://smartparens.readthedocs.io/en/stable/pair-management.html
  ;; https://smartparens.readthedocs.io/en/stable/permissions.html

  (sp-with-modes 'text-mode
    ;; Because of the `:actions' property, these just wrap things when the region
    ;; is active; it doesn't auto-insert when nothing is highlighted.
    (sp-local-pair "*" "*"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "_" "_"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "+" "+"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "|" "|"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "<" ">"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "=" "="   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "/" "/"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "$" "$"   :actions '(:add wrap autoskip) :when nil)
    (sp-local-pair "~" "~"   :actions '(:add wrap autoskip) :when nil))

  (sp-pair "*" "*" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "_" "_" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "+" "+" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "|" "|" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "<" ">" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "=" "=" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "/" "/" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p))
  (sp-pair "$" "$" :actions '(wrap) :when (list #'sp-in-comment-p #'sp-in-string-p #'sp-in-docstring-p)))

;;; embrace

(use-package! embrace
  :defer-incrementally t
  :init
  ;; HACK: see Doom source, this must be done early
  (define-advice embrace--setup-defaults (:after (&rest _) akn/extra-global-pairs)
    "Add some extra global pairs.

For example, I want \"d s _\" to delete the surrounding
underscores in all modes."
    (dolist (char '(?\* ?\_ ?\+ ?\| ?\= ?\/ ?\$ ?\: ?\# ?\% ?\- ?\~))
      (embrace-add-pair char (make-string 1 char) (make-string 1 char)))))

(use-package! evil-embrace
  :defer-incrementally (embrace))

;;; copy/pasting

;; TODO: deal with indenting when pasting
;; - https://github.com/jimeh/yank-indent - useful, but not what I want in Python, for example
;; - https://mbork.pl/2019-01-28_A_simple_tip_with_killing_and_yanking_code
;; - also, if I paste something mult-line while in a comment, I want the pasted thing to be commented - see `yank-in-context'
;; maybe s-v fancy indents and s-V doesn't
;; see `yank-transform-functions'

(use-package! indent-aux
  :defer-incrementally t
  :after-call doom-first-input-hook
  :config
  (kill-ring-deindent-mode)
  (defadvice! akn/disable-deindent-mode-a (fn &rest args)
    :around #'evil-yank
    :around #'evil-ex-yank
    :around #'evil-yank-line
    :around #'evil-yank
    :around #'evil-collection-magit-yank-whole-line
    :around #'kill-whole-line
    :around #'kill-line
    :around #'kill-matching-lines
    (if kill-ring-deindent-mode
        (unwind-protect
            (progn (kill-ring-deindent-mode -1)
                   (apply fn args))
          (kill-ring-deindent-mode 1))
      (apply fn args))))

;; (add-to-list 'yank-transform-functions #'akn/yank-match-indent-fn)

(defun akn/yank-reindent (&optional arg)
  (interactive "*P")
  (yank arg)
  (let ((mark-even-if-inactive transient-mark-mode))
    (indent-region (region-beginning) (region-end))))

(defun akn/common-starting-substring (&optional str1 str2 &rest rest)
  (cond
   ((null str1) "")
   ((null str2) str1)
   ((null rest)
    (cl-loop for char1 across str1
             for char2 across str2
             for num-chars upfrom 0
             while (eq char1 char2)
             finally return (substring str1 0 num-chars)))
   (t
    (apply #'akn/common-starting-substring
           (akn/common-starting-substring str1 str2)
           rest))))

(defun akn/indentation-of (line &optional include-other-chars-p)
  (and (string-match (rx bol (group (* (any " \t"))) (group (* not-newline))) line)
       (if include-other-chars-p
           (concat (match-string 1 line)
                   (make-string (string-width (match-string 2 line)) ?\s))
         (match-string 1 line))))

(defun akn/blank-line-p (line)
  (string-match-p (rx bos (* blank) eol) line))

(defun akn/not-blank-line-p (line)
  (not (akn/blank-line-p line)))

(defun akn/deindent-string (str)
  "TODO"
  (let* ((indent (akn/indentation-of
                  (apply #'akn/common-starting-substring
                         (seq-filter #'akn/not-blank-line-p (string-lines str)))))
         (indent-chars (length indent)))
    (cl-loop for line in (string-lines str nil 'keep-newlines)
             concat (if (akn/blank-line-p line)
                        "\n"
                      (substring line indent-chars)))))

(defun akn/yank-match-indent-fn (str)
  (setq str (akn/deindent-string str))
  (let* ((line-before-point (buffer-substring (pos-bol) (point)))
         (indent (akn/indentation-of line-before-point
                                     (apply #'derived-mode-p akn/lisp-like-modes)))
         (str-lines (string-lines str nil 'keep-newlines)))
    (cl-loop for line in str-lines
             for ind upfrom 0
             concat (if (= ind 0)
                        line
                      (concat indent line)))))

;;;; fix yank-pop

(let ((cmds (akn/cmds! (memq last-command '(evil-paste-after evil-paste-before evil-visual-paste)) #'evil-paste-pop
                       ;; consult-yank-pop errors when buffer is read-only.
                       ;; I want to be able to look at the kill ring even if I won't be able to actually use it.
                       (and (fboundp 'consult-yank-pop) (not buffer-read-only)) #'consult-yank-pop
                       #'yank-pop)))
  (map! :ng [remap yank-pop] cmds
        :ng [remap consult-yank-pop] cmds
        :ng [remap evil-paste-pop] cmds
        :ng [remap +default/yank-pop] cmds
        :ng "M-y" cmds))

;;; multiple cursors
(after! multiple-cursors
  (custom-set-faces!
    ;; https://github.com/doomemacs/themes/blob/master/doom-themes-base.el
    '(mc/cursor-bar-face
      :height 1 :background "magenta" :foreground "magenta"
      :family "Linux Libertine")))

(custom-set-faces!
  ;; https://github.com/doomemacs/themes/blob/master/doom-themes-base.el
  '(evil-mc-cursor-bar-face
    :height 1 :background "magenta" :foreground "magenta"
    :family "Linux Libertine"))

;;; TODO: make issue/PR
(define-advice +macos-open-with (:override (&optional app-name path) akn/a)
  "Send PATH to APP-NAME on OSX."
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 (rx "'") "\\'"
                 (or path (or (and (derived-mode-p 'dired-mode)
                                   (or (ignore-errors (dired-get-file-for-visit))
                                       dired-directory))
                              (and (derived-mode-p 'treemacs-mode)
                                   (ignore-errors (treemacs--prop-at-point :path)))
                              (and (derived-mode-p 'magit-mode)
                                   (ignore-errors (magit-file-at-point)))
                              (thing-at-point-file-at-point)
                              (buffer-file-name)
                              default-directory
                              (user-error "There's no file to open here")))
                 nil t)))
         (command (format "open %s '%s'"
                          (cond ((equal app-name "Finder") "-R")
                                (app-name (format "-a %s" (shell-quote-argument app-name)))
                                (t ""))
                          path)))
    (message "Running: %s" command)
    (shell-command command)))
(define-advice +macos/reveal-in-finder (:override () akn/a)
  (+macos-open-with "Finder"))

;;; goto-address-mode
(global-goto-address-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(define-advice goto-addr-mode--turn-on (:before-while (&rest _) akn/a)
  (not (derived-mode-p 'xwidget-webkit-mode)))

;;; Context menu
(use-package! mouse
  :defer 2
  :defer-incrementally t
  :init
  (map! "<mode-line> <down-mouse-3>" #'ignore-preserving-kill-region
        "<mode-line> <mouse-3>" #'ignore-preserving-kill-region
        "<mode-line> <C-down-mouse-1>" #'ignore-preserving-kill-region)
  :config
  (context-menu-mode))
;; right-clicking the status bar shouldn't close the window (#'mouse-delete-window)

;;; tooltips

;; Doom disables this
(use-package! tooltip
  :defer 2.1
  :unless akn/terminal-only-p
  :config
  (tooltip-mode))

;;; ligatures
;; Doom is now using #'mac-auto-operator-composition-mode on emacs-mac
;;

(when (modulep! :ui ligatures)
  (plist-put! +ligatures-extra-symbols
              ;; (first one is regular lambda, the default)
              ;; λx 𝛌x 𝜆x \x 𝝀x 𝝺x 𝞴x
              ;; :lambda "𝜆"
              ;; remove ligature +extra symbols I don't want in any mode
              :def nil
              :return nil :yield nil
              :false nil :true nil
              :dot nil
              :map nil
              ;; maybe
              ;; :for nil
              :composition nil
              :int nil :float nil :str nil :bool nil :list nil :tuple nil)

  (akn/after-hook! 'doom-first-buffer-hook
    (lambda ()
      (dolist (feature '(rjsx-mode typescript-mode web-mode (nodejs-repl-mode . nodejs-repl)
                         csharp-mode dart-mode scala-mode
                         (sh-mode . sh-script) (c++-mode . cc-mode) (c-mode . cc-mode)
                         (python-mode . python) (python-ts-mode . python) (python-base-mode . python)))
        (let ((pkg  (or (cdr-safe feature) feature))
              (mode (or (car-safe feature) feature)))
          (with-eval-after-load pkg
            (dolist (thing '("() =>" "null" "None" "for" "nullptr" "not" "or" "and"))
              (setf (alist-get mode +ligatures-extra-alist)
                    (assoc-delete-all thing (alist-get mode +ligatures-extra-alist)))))))))

  ;; Disable `prettify-symbols-mode' in terminal buffers, since it seems to
  ;; cause a bug where lines get duplicated when I'm moving between lines.
  (when akn/terminal-only-p
    (setq! +ligatures-extras-in-modes nil)))

;;; Jump back before scroll - and other jumping stuff
(map! "s-b"   #'better-jumper-jump-backward
      "M-s-b" #'better-jumper-jump-forward)

(after! better-jumper
  (setq! better-jumper-context 'buffer
         better-jumper-use-savehist t))

;; Using both buffer and window context
;;
;; (defvar better-jumper-context) ;silence warnings
;; (defadvice! akn/better-jumper-set-jump-a (origfn &rest args)
;;   :around #'better-jumper-set-jump
;;   (let ((better-jumper-context 'buffer))
;;     (apply origfn args))
;;   (let ((better-jumper-context 'window))
;;     (apply origfn args)))
;;
;; (defun akn/better-jumper-backward-in-buffer (&optional _count)
;;   (interactive)
;;   (let ((better-jumper-context 'buffer))
;;     (better-jumper-jump-backward)))
;;
;; (defun akn/better-jumper-forward-in-buffer (&optional _count)
;;   (interactive)
;;   (let ((better-jumper-context 'buffer))
;;     (better-jumper-jump-forward)))

(defconst akn/mwheel-scroll-commands
  (list #'mwheel-scroll
        #'mac-mwheel-scroll
        #'ultra-scroll
        #'ultra-scroll-mac
        #'pixel-scroll-down
        #'pixel-scroll-up))
(defconst akn/all-scroll-commands
  (append akn/mwheel-scroll-commands
          (list #'recenter
                #'recenter-top-bottom
                #'akn/recenter-top
                #'akn/recenter-bottom
                #'scroll-bar-drag
                #'scroll-bar-scroll-up
                #'scroll-bar-scroll-down
                #'scroll-down
                #'scroll-down-line
                #'scroll-down-command
                #'scroll-up
                #'scroll-up-line
                #'scroll-up-command
                #'scroll-left
                #'scroll-right
                #'scroll-other-window
                #'scroll-other-window-down
                #'cua-scroll-up
                #'cua-scroll-down
                #'Info-scroll-up
                #'Info-scroll-down
                #'calc-scroll-up
                #'calc-scroll-down
                #'symex--scroll-up
                #'symex--scroll-down
                #'symex--evil-scroll-down
                #'evil-scroll-up
                #'evil-scroll-down
                #'evil-scroll-left
                #'evil-scroll-right
                #'evil-scroll-line-up
                #'evil-scroll-line-down
                #'evil-scroll-page-up
                #'evil-scroll-page-down)))
(add-hook 'pre-command-hook #'akn/set-jump-before-mwheel-scroll-h 95)
(defun akn/set-jump-before-mwheel-scroll-h ()
  (when (and (memq this-command akn/mwheel-scroll-commands)
             (not (memq last-command akn/mwheel-scroll-commands)))
    (better-jumper-set-jump)))

;; (defun akn/better-jumper-jump-backward-in-buffer (&optional count)
;;   (interactive)
;;   (while (> (or count 1) 0)
;;     (let ((successful))))
;;   (let ((num-jumps 0))
;;     (cl-loop while (< num-jumps count) do
;;       (let* ((struct (better-jumper--get-struct))
;;              (idx (better-jumper-jump-list-struct-idx struct)))
;;         (when (= idx -1)
;;           (setq idx 0)
;;           (setf (better-jumper-jump-list-struct-idx struct) 0)
;;           (better-jumper--push))
;;         (let ((jump-list (better-jumper--get-jump-list context)))
;;           (cl-incf idx)
;;           (let* ((size (ring-length jump-list)))
;;             (when (not (and (< idx size) (>= idx 0)))
;;               (cl-return))
;;             ;; actual jump
;;             (run-hooks 'better-jumper-pre-jump-hook)
;;             (let* ((marker-table (better-jumper--get-marker-table context))
;;                    (place (ring-ref jump-list idx))
;;                    (file-name (nth 0 place))
;;                    (pos (nth 1 place))
;;                    (marker-key (nth 2 place))
;;                    (marker (gethash marker-key marker-table)))
;;               (setq better-jumper--jumping t)
;;               (when better-jumper-use-evil-jump-advice
;;                 (setq evil--jumps-jumping-backward t))
;;               (when (equal buffer-file-name file-name)
;;                 (if (string-match-p better-jumper--buffer-targets file-name)
;;                     (switch-to-buffer file-name)
;;                   (find-file file-name))
;;                 (if (and marker (marker-position marker))
;;                     (goto-char marker)
;;                   (goto-char pos)
;;                   (puthash marker-key (point-marker) marker-table)))
;;               (setf (better-jumper-jump-list-struct-idx (better-jumper--get-struct context)) idx)
;;               (setq better-jumper--jumping nil)
;;               (run-hooks 'better-jumper-post-jump-hook))))))))
;; (defun akn/better-jumper-jump-forward-in-buffer (&optional count)
;;   (interactive)
;;   (let* ((count (or count 1))
;;          (struct (better-jumper--get-struct))
;;          (idx (better-jumper-jump-list-struct-idx struct)))
;;     (when (= idx -1)
;;       (setq idx 0)
;;       (setf (better-jumper-jump-list-struct-idx struct) 0)
;;       (better-jumper--push))
;;     (better-jumper--jump idx (- 0 count))))


;; Jumping libraries
;; =================
;; built-in: mark ring, global mark ring, registers, bookmarks
;; https://github.com/Overdr0ne/gumshoe
;; https://github.com/alphapapa/dogears.el
;; https://github.com/blue0513/point-history
;; https://github.com/dgutov/point-stack/
;; https://github.com/gilbertw1/better-jumper
;; https://github.com/gitrj95/trail.el
;; https://github.com/liuyinz/binky.el
;; https://github.com/rolandwalker/back-button
;; https://github.com/tcw165/history
;; https://elpa.gnu.org/packages/cursor-undo.html
;; https://www.emacswiki.org/emacs/BookmarkPlus#AutomaticIdle-PeriodBookmarking

;; https://old.reddit.com/r/emacs/comments/17g17m2/bookmark_or_dogears_or/
;; https://old.reddit.com/r/emacs/comments/pt8kod/gumshoe_20_my_first_package_in_melpa/


;;; bookmarks

(setq!
 ;; Save to bookmarks file every time bookmarks change.
 bookmark-save-flag 1
 ;; If the bookmark file changes on disk, reload it without prompting.
 bookmark-watch-bookmark-file 'silent)

;;; Tabs & spaces

(setq-default tab-width 2)

(defvar-local akn/evil-shift-width-different-from-tab-width nil
  "If this is non-nil, `evil-shift-width' won't be set equal to `tab-width'.")
(define-advice +evil-adjust-shift-width-h (:before-while (&rest _) akn/a)
  (not akn/evil-shift-width-different-from-tab-width))

(defconst akn/lisp-like-modes
  '(lisp-mode
    emacs-lisp-mode
    scheme-mode
    clojure-mode
    racket-mode
    hy-mode
    fennel-mode
    lfe-mode
    common-lisp-mode
    lisp-interaction-mode
    ielm-mode
    slime-repl-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    inferior-hy-mode
    inferior-fennel-mode
    inferior-lfe-mode
    inferior-common-lisp-mode
    inferior-lisp-interaction-mode
    inferior-slime-mode
    helpful-mode))
(def-project-mode! akn/lisp-like-mode
  :modes akn/lisp-like-modes
  :on-enter (setq-local tab-width 8
                        evil-shift-width 1
                        akn/evil-shift-width-different-from-tab-width t
                        standard-indent 1
                        evil-shift-round nil
                        backward-delete-char-untabify-method nil
                        dtrt-indent-hook-generic-mapping-list nil))
(add-hook 'helpful-mode-hook #'akn/lisp-like-mode)
(map! :map akn/lisp-like-mode-map
      ;; like in DrRacket
      :nvieomrg "M-<left>"      (akn/cmds! (bound-and-true-p smartparens-mode) #'sp-backward-sexp
                                           #'backward-sexp)
      :nvieomrg "M-<right>"     (akn/cmds! (bound-and-true-p smartparens-mode) #'sp-forward-sexp
                                           #'forward-sexp)
      :nvieomrg "M-<backspace>" (akn/cmds! (bound-and-true-p smartparens-mode) #'sp-backward-kill-sexp
                                           #'backward-kill-sexp))
(after! smartparens
  (define-advice sp-forward-sexp (:around (fn &rest args) akn/a)
    (prog1
        (if (and (modulep! :editor evil) (memq evil-state '(normal motion)))
            (let ((state-before evil-state)
                  (evil-move-cursor-back t)
                  ret)
              (if (or (seq-contains-p "[({<#'" (char-after))
                      (and (memq (char-before) '(?\s ?\r ?\n ?\t nil))
                           (not (memq (char-after (1+ (point))) '(?\s ?\r ?\n ?\t nil)))))
                  (evil-insert 1)
                (evil-append 1))
              (setq ret (apply fn args))
              (evil-change-state state-before)
              ret)
          (apply fn args))))
  (define-advice sp-backward-sexp (:around (fn &rest args) akn/a)
    (prog1
        (if (and (modulep! :editor evil) (memq evil-state '(normal motion)))
            (let ((state-before evil-state)
                  (evil-move-cursor-back nil)
                  ret)
              (if (or (seq-contains-p "])}>" (char-after))
                      (and (memq (char-after (1+ (point))) '(?\s ?\r ?\n ?\t nil))
                           (not (memq (char-before) '(?\s ?\r ?\n ?\t nil)))))
                  (evil-append 1)
                (evil-insert 1))
              (setq ret (apply fn args))
              (evil-change-state state-before)
              ret)
          (apply fn args))
      (while (memq (char-before) '(?# ?'))
        (backward-char)))))

(map! :map (lisp-mode-shared-map smartparens-strict-mode-map)
      ;; instead of backward-delete-char-untabify
      "DEL" (akn/defun akn/backward-delete-char1 ()
              (interactive)
              (let ((tab-width 1))
                (call-interactively #'backward-delete-char))))

;;; ws-butler
(after! ws-butler
  ;; Doom disables this by default:
  ;;   "ws-butler normally preserves whitespace in the buffer (but strips it from
  ;;    the written file). While sometimes convenient, this behavior is not
  ;;    intuitive. To the average user it looks like whitespace cleanup is failing,
  ;;    which causes folks to redundantly install their own."
  (setq! ws-butler-keep-whitespace-before-point t)

  ;; ws-butler doesn't seem to work too well in polymodes
  (setq-hook! '(polymode-init-inner-hook
                polymode-init-host-hook
                polymode-after-switch-buffer-hook
                polymode-before-switch-buffer-hook)
    ws-butler-keep-whitespace-before-point nil)

  (setq-hook! 'akn/auto-save-visited-local-mode-hook
    ws-butler-keep-whitespace-before-point t))

;;; parinfer-rust

(when-let* (((modulep! :editor parinfer))
            ((not (akn/existing-file (bound-and-true-p parinfer-rust-library))))
            (lib (or
                  ;; nix profile install 'nixpkgs#parinfer-rust-emacs'
                  (car (doom-glob "~/.nix-profile/lib/libparinfer_rust*"))
                  ;; on NixOS or nix-darwin if installed per-user
                  (and-let* ((executable (executable-find "parinfer-rust"))
                             (executable-realpath (file-truename executable))
                             (dir (file-name-directory executable-realpath))
                             ((car (doom-glob dir "../lib/libparinfer_rust*")))))
                  ;; on NixOS if installed globally
                  (car (doom-glob "/nix/var/nix/profiles/system/sw/lib/libparinfer_rust*"))))
            ((not (file-directory-p lib))))
  (setq parinfer-rust-library (expand-file-name lib)
        parinfer-rust-auto-download nil))

(after! parinfer-rust-mode
  (setq! parinfer-rust-buffer-replace-strategy 'safe))

(map! [remap parinfer-rust-toggle-disable] #'akn/parinfer-toggle
      :map (lisp-mode-shared-map parinfer-rust-mode-map akn/lisp-like-mode-map)
      :localleader "P" #'akn/parinfer-toggle)
(defun akn/parinfer-on-p ()
  (and (bound-and-true-p parinfer-rust-mode))) ;(not parinfer-rust--disable)))
(defun akn/parinfer-on ()
  (when (not parinfer-rust-mode) (parinfer-rust-mode))
  (setq-local parinfer-rust--disable nil))
(defun akn/parinfer-off ()
  (parinfer-rust-mode -1))
(defun akn/parinfer-toggle ()
  (interactive)
  (if (akn/parinfer-on-p)
      (akn/parinfer-off)
    (akn/parinfer-on))
  (message "parinfer-rust %s" (if (akn/parinfer-on-p) "enabled" "disabled")))

(define-advice parinfer-rust--check-for-issues (:before-until (&rest _) akn/a)
  ;; conditions where we shouldn't run `parinfer-rust--check-for-issues'
  (or (and buffer-read-only (not (buffer-modified-p)))
      (and buffer-file-name (string-match-p (rx "config/emacs/") buffer-file-name))))

(after! parinfer-rust-mode
  (pushnew! parinfer-rust-treat-command-as
            '(evil-open-below . "paren")
            '(evil-open-above . "paren")
            '(newline-and-indent . "paren")
            '(+default/newline-above . "paren")
            '(+default/newline-below . "paren")
            '(+evil/shift-right . "indent")
            '(+evil/shift-left . "indent")
            '(evil-shift-right-line . "smart")
            '(evil-shift-left-line . "smart")
            '(evil-shift-right . "smart")
            '(evil-shift-left . "smart")
            '(doom/backward-kill-to-bol-and-indent . "smart")
            '(evil-delete-back-to-indentation . "smart")
            '(sp-comment . "paren")
            '(sp-newline . "paren")
            '(sp-up-sexp . "paren")
            '(sp-copy-sexp . "paren")
            '(sp-down-sexp . "paren")
            '(sp-emit-sexp . "paren")
            '(sp-join-sexp . "paren")
            '(sp-kill-sexp . "paren")
            '(sp-kill-word . "paren")
            '(sp-mark-sexp . "paren")
            '(sp-next-sexp . "paren")
            '(sp-clone-sexp . "paren")
            '(sp-raise-sexp . "paren")
            '(sp-split-sexp . "paren")
            '(sp-wrap-curly . "paren")
            '(sp-wrap-round . "paren")
            '(sp-absorb-sexp . "paren")
            '(sp-cheat-sheet . "paren")
            '(sp-delete-char . "paren")
            '(sp-delete-sexp . "paren")
            '(sp-delete-word . "paren")
            '(sp-end-of-sexp . "paren")
            '(sp-kill-region . "paren")
            '(sp-kill-symbol . "paren")
            '(sp-region-ok-p . "paren")
            '(sp-rewrap-sexp . "paren")
            '(sp-splice-sexp . "paren")
            '(sp-unwrap-sexp . "paren")
            '(sp-wrap-cancel . "paren")
            '(sp-wrap-square . "paren")
            '(sp-change-inner . "paren")
            '(sp-forward-sexp . "paren")
            '(sp-indent-defun . "paren")
            '(sp-backward-sexp . "paren")
            '(sp-delete-region . "paren")
            '(sp-delete-symbol . "paren")
            '(sp-html-next-tag . "paren")
            '(sp-previous-sexp . "paren")
            '(sp-convolute-sexp . "paren")
            '(sp-forward-symbol . "paren")
            '(sp-narrow-to-sexp . "paren")
            '(sp-transpose-sexp . "paren")
            '(sp-backward-symbol . "paren")
            '(sp-describe-system . "paren")
            '(sp-kill-whole-line . "paren")
            '(sp-add-to-next-sexp . "paren")
            '(sp-backward-up-sexp . "paren")
            '(sp-change-enclosing . "paren")
            '(sp-end-of-next-sexp . "paren")
            '(sp-kill-hybrid-sexp . "paren")
            '(sp-push-hybrid-sexp . "paren")
            '(sp-beginning-of-sexp . "paren")
            '(sp-forward-barf-sexp . "paren")
            '(sp-html-previous-tag . "paren")
            '(sp-prefix-tag-object . "paren")
            '(sp-select-next-thing . "paren")
            '(sp-slurp-hybrid-sexp . "paren")
            '(sp-backward-barf-sexp . "paren")
            '(sp-backward-copy-sexp . "paren")
            '(sp-backward-down-sexp . "paren")
            '(sp-backward-kill-sexp . "paren")
            '(sp-backward-kill-word . "paren")
            '(sp-dedent-adjust-sexp . "paren")
            '(sp-extract-after-sexp . "paren")
            '(sp-forward-slurp-sexp . "paren")
            '(sp-forward-whitespace . "paren")
            '(sp-indent-adjust-sexp . "paren")
            '(sp-prefix-pair-object . "paren")
            '(sp-backward-slurp-sexp . "paren")
            '(sp-backward-whitespace . "paren")
            '(sp-extract-before-sexp . "paren")
            '(sp-show-enclosing-pair . "paren")
            '(sp-swap-enclosing-sexp . "paren")
            '(sp-add-to-previous-sexp . "paren")
            '(sp-backward-delete-char . "paren")
            '(sp-backward-delete-sexp . "paren")
            '(sp-backward-delete-word . "paren")
            '(sp-backward-kill-symbol . "paren")
            '(sp-backward-unwrap-sexp . "paren")
            '(sp-end-of-previous-sexp . "paren")
            '(sp-prefix-symbol-object . "paren")
            '(sp-use-paredit-bindings . "paren")
            '(sp-forward-parallel-sexp . "paren")
            '(sp-prefix-save-excursion . "paren")
            '(sp-select-previous-thing . "paren")
            '(sp-transpose-hybrid-sexp . "paren")
            '(sp-backward-delete-symbol . "paren")
            '(sp-backward-parallel-sexp . "paren")
            '(sp-beginning-of-next-sexp . "paren")
            '(sp-highlight-current-sexp . "paren")
            '(sp-skip-forward-to-symbol . "paren")
            '(sp-skip-backward-to-symbol . "paren")
            '(sp-use-smartparens-bindings . "paren")
            '(sp-beginning-of-previous-sexp . "paren")
            '(sp-remove-active-pair-overlay . "paren")
            '(sp-select-next-thing-exchange . "paren")
            '(sp-splice-sexp-killing-around . "paren")
            '(sp-splice-sexp-killing-forward . "paren")
            '(sp-splice-sexp-killing-backward . "paren")
            '(sp-select-previous-thing-exchange . "paren")))

;;; backups and auto-save

(add-hook! '(age-encryption-mode-hook auto-encryption-mode-hook)
  (defun akn/no-auto-save-or-backup ()
    (setq-local backup-inhibited t)
    (auto-save-mode -1)))

;;;; better-backup (my library)

(add-hook! '(find-file-hook akn/new-file-mode-hook)
           #'better-backup-buffer-mode)
(add-hook! 'before-save-hook
  (defun akn/better-backup-buffer-mode-maybe-h ()
    (when (and buffer-file-name (not (bound-and-true-p better-backup-buffer-mode)))
      (better-backup-buffer-mode))))
(akn/after-idle! ((* 60 27) :each-idle t)
  (let ((default-directory better-backup-directory))
    (when (and (file-exists-p default-directory)
               (executable-find "fclones"))
      (akn/run-command "pwd && fclones group . | fclones link" :shell t :output-buffer " *dedupe-better-backup*"))))

;;;; backups (disabled by doom)

;; saved in ~/.cache/doom/backup/ (and ~/.cache/doom/tramp-backup/ on remote machines)
;; (even though doom disables backups by default, it also sets some settings)
;; NOTE: this only applies to  - see `vc-make-backup-files'.
(setq make-backup-files t
      ;; whether to back up version-controlleda files
      vc-make-backup-files nil
      version-control t
      kept-new-versions 10
      kept-old-versions 5
      dired-kept-versions kept-new-versions)

(setq backup-enable-predicate
      (akn/defun akn/backup-enable-predicate (name)
        (and (normal-backup-enable-predicate name)
             ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
             (let ((method (file-remote-p name 'method)))
               (not
                (and (stringp method)
                     (member method '("su" "sudo"))))))))

(defun akn/select-backup-file (&optional orig-file)
  (interactive (list buffer-file-name))
  (and orig-file
       (let ((files (seq-filter #'file-exists-p (file-backup-file-names orig-file))))
         (cond
          ((null files) nil)
          ((null (cdr files)) (car files))
          (t (akn/completing-read files
                                  :prompt "choose a backup file: "
                                  :default (file-newest-backup buffer-file-name)
                                  :category 'file))))))

(defvar-local akn/backup-original-file nil)
(defun akn/find-backup-file (&optional orig-file)
  (interactive (list buffer-file-name))
  (setq orig-file (or orig-file akn/backup-original-file buffer-file-name))
  (if-let* ((file (akn/select-backup-file orig-file)))
      (let ((m major-mode))
        (find-file-read-only file)
        (funcall m)
        (setq-local akn/backup-original-file orig-file)
        (akn/backup-file-mode))
    (cond
     ((require 'magit-wip nil t)
      (akn/find-magit-wip-file orig-file))
     (t
      (user-error "No backup files found")))))

(define-minor-mode akn/backup-file-mode
  "A minor mode for viewing backup files."
  :lighter "Backup"
  :group 'akn
  (cond
   (akn/backup-file-mode
    (read-only-mode)
    (view-mode))))

(defun akn/find-magit-wip-file (orig-file)
  (interactive (list buffer-file-name))
  (with-current-buffer (if orig-file (find-file-noselect orig-file) (current-buffer))
    (let ((args (nconc (list (or (magit-get-current-branch) "HEAD"))
                       (magit-log-arguments)
                       (list (prefix-numeric-value current-prefix-arg)))))
      (when orig-file
        (setf (nth 2 args) (list orig-file)))
      (apply #'magit-wip-log-current args))))

;;;; auto-save

(setq auto-save-interval 50
      auto-save-timeout 10)

(defun akn/delete-auto-save-file ()
  "Delete auto-save file for current buffer if `delete-auto-save-files' is t.
Normally delete only if the file was written by this Emacs since
the last real save, but optional arg FORCE non-nil means delete anyway.

Mostly copied from `delete-auto-save-file-if-necessary'."
  (interactive)
  (cond ((not (and buffer-auto-save-file-name ; delete-auto-save-files
                   (not (string= buffer-file-name buffer-auto-save-file-name))
                   (file-exists-p buffer-auto-save-file-name)))
         (when (called-interactively-p 'any)
           (user-error "No auto-save file to delete"))
         nil)
        ((not (or (recent-auto-save-p)
                  (y-or-n-p (format "Delete auto-save file %s? It wasn't made recently." buffer-auto-save-file-name))))
         nil)
        (t
         (prog1
             (condition-case ()
                 (progn
                   (delete-file buffer-auto-save-file-name)
                   (when (called-interactively-p 'any)
                     (message "Deleted auto-save-file"))
                   t)
               (file-error nil))
           (set-buffer-auto-saved)))))

(defun akn/find-auto-save-file ()
  "Open the auto-save file for the current buffer."
  (interactive)
  (if (and buffer-auto-save-file-name
           (file-exists-p buffer-auto-save-file-name))
      (let ((m major-mode))
        (find-file-read-only buffer-auto-save-file-name)
        (funcall m)
        (view-mode))
    (message "Not auto save file found")))

;;;; lockfiles (disabled by doom)

(setq create-lockfiles t
      remote-file-name-inhibit-locks nil)

;; https://old.reddit.com/r/emacs/comments/wkh9cq/emacs_creates_lock_files_despite_being_told_not_to/
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
;; https://github.com/emacscollective/no-littering/blob/main/README.org#lock-files
(let ((dir "~/.cache/doom/lockfiles/"))
  (make-directory dir t)
  (setq lock-file-name-transforms `((".*" ,dir t))))

;;;; doom bugfixes

(setq
 ;; TODO: PR or issue: by default, Doom sets it to an absolute path
 ;; ("/Users/ajainelson/.config/emacs/.local/cache/backup/"), which doesn't
 ;; exist on Linux. Maybe use `abbreviate-file-name'.
 tramp-backup-directory-alist (list (cons "." "~/.cache/doom/tramp-backup/"))
 tramp-auto-save-directory "~/.cache/doom/tramp-autosave/")

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
           (concat (or (and (string-match-p "/tramp-autosave/" buffer-file-name) (file-name-directory buffer-file-name)) "")
                   (sha1 buffer-file-name)))))
    (funcall fn)))

;;; formatting
(setq-hook! (html-mode
             css-mode
             web-mode
             markdown-mode
             js-mode
             json-mode
             typescript-mode
             typescript-tsx-mode)
  +format-with-lsp nil)

(defun akn/prettier-p ()
  (unless (akn/file-remote-p default-directory)
    (or (locate-dominating-file default-directory ".prettierrc")
        (locate-dominating-file default-directory ".prettierrc.json")
        (locate-dominating-file default-directory ".prettierrc.yml")
        (locate-dominating-file default-directory ".prettierrc.yaml")
        (locate-dominating-file default-directory ".prettierrc.json5")
        (locate-dominating-file default-directory ".prettierrc.js")
        (locate-dominating-file default-directory "prettier.config.js")
        (locate-dominating-file default-directory ".prettierrc.mjs")
        (locate-dominating-file default-directory "prettier.config.mjs")
        (locate-dominating-file default-directory ".prettierrc.cjs")
        (locate-dominating-file default-directory "prettier.config.cjs")
        (locate-dominating-file default-directory ".prettierrc.js")
        (locate-dominating-file default-directory ".prettierrc.js")
        (locate-dominating-file default-directory ".prettierrc.toml")
        (locate-dominating-file default-directory (lambda (dir)
                                                    (when-let* ((package-json (akn/existing-file (expand-file-name "package.json" dir)))
                                                                (json (ignore-errors (json-read-file package-json))))
                                                      (not (eq (alist-get 'prettier json 'not-found)
                                                               'not-found))))))))

(add-hook! '(html-mode-hook
             css-mode-hook
             web-mode-hook
             markdown-mode-hook
             js-mode-hook
             json-mode-hook
             typescript-mode-hook)
           :append
           (defun akn/maybe-prettier ()
             (when (and (not (bound-and-true-p akn/format-on-save-mode))
                        (akn/prettier-p))
               (setq-local +format-with 'prettier)
               (akn/format-on-save-mode))))

;;; tailwind.css

(use-package! lsp-tailwindcss
  :if (modulep! :tools lsp -eglot)
  :init
  (setq lsp-tailwindcss-add-on-mode t)

  (defun akn/require-tailwind ()
    (or (require 'lsp-tailwindcss nil t)
        (message "(lsp-tailwindcss not installed)")))

  (add-hook! '(html-mode-hook
               css-mode-hook
               web-mode-hook
               markdown-mode-hook
               js-mode-hook
               json-mode-hook
               typescript-mode-hook
               typescript-tsx-mode-hook)
    (defun akn/maybe-require-tailwind ()
      (when (or (locate-dominating-file default-directory "tailwind.config.js")
                (locate-dominating-file default-directory "tailwind.config.cjs")
                (locate-dominating-file default-directory "tailwind.config.mjs")
                (locate-dominating-file default-directory "tailwind.config.ts")
                (locate-dominating-file default-directory "tailwind.config.cts")
                (locate-dominating-file default-directory "tailwind.config.mts")
                (locate-dominating-file default-directory (lambda (dir)
                                                            (when-let* ((package-json (akn/existing-file (expand-file-name "package.json" dir)))
                                                                        (json (ignore-errors (json-read-file package-json)))
                                                                        (deps (let-alist json
                                                                                (append .dependencies
                                                                                        .devDependencies
                                                                                        .peerDependencies
                                                                                        .bundledDependencies
                                                                                        .optionalDependencies))))
                                                              (alist-get 'tailwindcss deps)))))
        (akn/require-tailwind)))))

;;; doom reloading and upgrading
(setq doom-upgrade-command
      (format "%s upgrade-doom -B --yes --fail-if-up-to-date"
              ;; /usr/bin/env doesn't exist on Android
              (if (featurep :system 'android)
                  "sh %s"
                "%s")))

;;; displaying page breaks / form feeds (^L)
;; https://en.wikipedia.org/wiki/Page_break#Form_feed
;; "This convention is predominantly used in Lisp code, and is also seen in C and Python source code."
(add-hook! '(akn/lisp-like-mode-hook c-mode-hook c++-mode-hook python-mode-hook)
           ;; from page-break-lines package
           #'page-break-lines-mode)

;;; sibling files
(akn/incrementally! ()
  (dolist (extensions (list (rx (or "tex" "tex.pm" "typ" "pdf" "md" "org" "html" "Rmd" "qmd" "scrbl"))
                            (rx (? "res.") (? (or "m" "c")) (or "js" "ts" "d.ts") (? "x"))))
    (add-to-list 'find-sibling-rules (list (rx "/" (group (1+ (not (or "/" ".")))) "." (regexp extensions) eos)
                                           (rx     (backref 1                    ) "." (regexp extensions) eos))))

  (add-to-list
   'find-sibling-rules
   (list (rx "/" (group (1+ (not (or "/" ".")))) "." (or "spec" "test" (+ anychar)) "." (group (1+ (not "/"))) eos)
         (rx     (backref 1                    )                                    "." (backref            2) eos)))
  (add-to-list
   'find-sibling-rules
   (list (rx "/" (group (1+ (not (or "/" "."))))                                    "." (group (1+ (not "/"))) eos)
         (rx     (backref 1                    ) "." (or "spec" "test" (+ anychar)) "." (backref            2) eos))))

;;; midnight-mode
(use-package! midnight
  :defer 3600
  :init
  (let ((newval "5:00am"))
    (unless (equal (bound-and-true-p midnight-delay) newval)
      (setq! midnight-delay newval)))
  :config
  (setq! clean-buffer-list-delay-special (* 60 60 6))
  ;; https://old.reddit.com/r/emacs/comments/15rubhr/til_midnight_mode/
  (pushnew! clean-buffer-list-kill-regexps
            (rx bos "magit-" (or "process" "diff"))
            (rx bos "*helpful"))
  (akn/advice-remove #'clean-buffer-list :around #'doom-shut-up-a)
  (midnight-mode))

;;; deft
(use-package! deft
  :config
  (setq! deft-directory "~/Documents/obsidian-vault"
         deft-default-extension "md"
         deft-recursive t))

;;; word wrap
;; enable word-wrap (almost) everywhere
;; toggle: SPC t w
(use-package! adaptive-wrap
  :when (modulep! :editor word-wrap)
  :defer-incrementally t
  :ghook ('doom-first-buffer-hook #'+global-word-wrap-mode)
  :config
  (add-to-list '+word-wrap-disabled-modes 'elfeed-search-mode)
  (add-to-list '+word-wrap-disabled-modes 'org-agenda-mode)
  (+global-word-wrap-mode +1))
;; (add-to-list '+word-wrap-disabled-modes _)

(defun akn/disable-slow-adaptive-fill-functions-a (fn &rest args)
  (cond
   ((eq adaptive-fill-function #'rust-find-fill-prefix)
    (let ((adaptive-fill-function nil)
          (adaptive-fill-regexp (rx
                                 (* (any "\t "))
                                 (* (group
                                     (? (seq "//" (* "/")))
                                     (* (any "\t "))))))
          (adaptive-fill-first-line-regexp ""))
      (substring-no-properties
       (apply fn args))))
   (t (apply fn args))))
(after! 'rust-prog-mode
  (advice-add #'adaptive-wrap-fill-context-prefix :around #'akn/disable-slow-adaptive-fill-functions-a))

;;; isearch
(after! isearch
  (map! :map isearch-mode-map
        [remap evil-normal-state] #'isearch-cancel
        [remap evil-force-normal-state] #'isearch-cancel)
  (add-hook! 'doom-escape-hook
    (defun akn/isearch-remove-overlays-h ()
      (when isearch-opened-overlays
        (isearch-clean-overlays)
        t)))

  (setq! isearch-allow-scroll 'unlimited)
  (dolist (scroll-cmd akn/all-scroll-commands)
    (put scroll-cmd 'isearch-scroll t))

  (setq! isearch-allow-motion nil)
  (setf (get #'scroll-up   'isearch-motion) nil)
  (setf (get #'scroll-down 'isearch-motion) nil)
  ;; (setf (get #'evil-scroll-page-down 'isearch-motion) (get #'scroll-up   'isearch-motion))
  ;; (setf (get #'evil-scroll-page-up   'isearch-motion) (get #'scroll-down 'isearch-motion))

  (setq! isearch-yank-on-move 'shift
         isearch-lazy-count t
         isearch-lazy-highlight t
         isearch-resume-in-command-history t
         search-ring-max 128
         regexp-search-ring-max 128))

;;; fix rectangular selection in normal mode
;; TODO: make it actually do visual block
;; https://github.com/emacs-evil/evil/issues/1899
(map! :nvm [C-M-down-mouse-1] (akn/defun akn/mouse-drag-region-rectangle (start-event)
                                (interactive "e")
                                (evil-insert-state)
                                (mouse-drag-region-rectangle start-event))
      :nvm [C-M-drag-mouse-1] #'ignore-preserving-kill-region
      :nvm [C-M-mouse-1]      #'mouse-set-point

      "H-SPC" (cmd! (evil-insert-state) (call-interactively #'set-rectangular-region-anchor)))

;;; speedrect
(use-package! speedrect
  :defer-incrementally (rect calc subr-x compat cl-lib)
  :after-call rectangle-mark-mode-on-hook
  :config
  (speedrect-mode))

;;; company (completion)
(after! company
  ;; In org-mode, don't autocomplete unless triggered.
  ;; https://emacs.stackexchange.com/a/32523
  ;; https://company-mode.github.io/manual/Customization.html#index-company_002didle_002ddelay
  (after! org
    (setq! company-idle-delay
           (lambda () (if (string-equal major-mode "org-mode") nil 0.2)))))

;;; gmch alternative (experimental)
;; (when nil
;;   (defvar akn/gcmh-timer nil)
;;   (define-minor-mode akn/gcmh-mode
;;     ""
;;     :global t
;;     (if akn/gcmh-mode
;;         (progn
;;           (akn/gcmh-start))))
;;   (defun akn/gcmh-start ()
;;     (when)))

;;; custom
(after! wid-edit
  (when (modulep! :editor evil)
    (evil-make-overriding-map widget-keymap)
    (evil-make-overriding-map widget-link-keymap)))
(after! cus-edit
  (map! :map Custom-mode-map
        [remap save-buffer] #'Custom-save))

;;; xterm-color
(after! compile
  (require 'xterm-color)

  ;; https://github.com/atomontage/xterm-color?tab=readme-ov-file#compilation-buffers
  ;; WARNING: "This compilation-mode configuration will break ag.el and rg.el,
  ;; since these packages expect ANSI control sequences to be part of
  ;; compilation output so that they can be used for matching."
  (add-to-list 'compilation-environment "TERM=xterm-256color"))
(defadvice! akn/xterm-color--advice-compilation-filter-a (f proc string)
  :around #'compilation-filter
  (funcall f proc (xterm-color-filter string)))

;; TODO: fix echoing and prompt
;; (after! shell
;;   (require 'xterm-color)
;;
;;   ;; https://github.com/atomontage/xterm-color?tab=readme-ov-file#comint
;;   ;; "Also set TERM accordingly (xterm-256color) in the shell itself."
;;
;;   (add-hook! 'shell-mode-hook
;;     (defun akn/xterm-color--shell ()
;;       (when (and (buffer-name) (string-match-p (rx "*doom:shell") (buffer-name)))
;;         (setq-local comint-output-filter-functions
;;                     (remove #'ansi-color-process-output comint-output-filter-functions))
;;         ;; Disable font-locking in this buffer to improve performance
;;         (font-lock-mode -1)
;;         ;; Prevent font-locking from being re-enabled in this buffer
;;         (make-local-variable 'font-lock-function)
;;         (setq font-lock-function (lambda (_) nil))
;;         (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)
;;
;;         (comint-simple-send (get-buffer-process (current-buffer)) "TERM=xterm-256color")))))

(after! eshell
  (require 'xterm-color)

  ;; https://github.com/atomontage/xterm-color?tab=readme-ov-file#eshell
  (add-hook! 'eshell-before-prompt-hook
    (defun akn/xterm-color--eshell ()
      (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
  (setq eshell-output-filter-functions (remove #'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))
;;; elisp
(map!
 (:mode (emacs-lisp-mode lisp-interaction-mode)
  "C-c C-c" #'eval-buffer
  "C-c k" #'akn/insert-keybinding-string
  :mnie "s-<return>" #'akn/eval-defun-and-move-down
  :v "s-<return>" #'akn/eval-region-and-move-down
  ;; SPC m k
  :localleader "k" #'akn/insert-keybinding-string))

(defun akn/eval-defun-and-move-down ()
  (interactive)
  (call-interactively #'eval-defun)
  (beginning-of-defun -1))
(defun akn/eval-region-and-move-down ()
  (interactive)
  (call-interactively #'eval-region)
  (when (modulep! :editor evil)
    (evil-visual-goto-end)
    (evil-force-normal-state))
  (forward-line))

(defun akn/elisp-thing-at-point-or-region (&rest args)
  (or (apply #'doom-thing-at-point-or-region args)
      (save-excursion
        (while (memq (char-after) (list ?\# ?\' ?\( ?\) ?\[ ?\] ?\{ ?\} ?\s ?\t ?\" ?\:))
          (right-char))
        (apply #'doom-thing-at-point-or-region args))))
(defun akn/elisp-lookup-documentation (&rest args)
  (interactive (list (akn/elisp-thing-at-point-or-region)
                     current-prefix-arg))
  (apply #'+lookup/documentation args))
(map! :map (akn/lisp-like-mode-map help-mode-map)
      :nvm "K" #'akn/elisp-lookup-documentation)

(set-lookup-handlers! '(help-mode Info-mode)
  :definition    #'+emacs-lisp-lookup-definition
  :documentation #'+emacs-lisp-lookup-documentation)

;;; commands for writing emacs configs
(map!
 ;; previously #'doom/open-private-config
 "s-," #'akn/open-config
 "s-<" #'akn/reload-config)

(defalias 'akn/change-inner-quotes
  (kmacro "c i \""))
(defun akn/has-quote-in-rest-of-line-p ()
  (seq-contains-p (buffer-substring-no-properties (point) (pos-eol))
                  ?\"))
(defun akn/insert-keybinding-string (key-sequence)
  "If you don't remember the emacs notation for keybindings
(e.g. that command-shift-f is s-F), then this should help!"
  (interactive
   (list (read-key-sequence "Press key: ")))
  (akn/evil-save-state
   (evil-force-normal-state)
   (if (akn/has-quote-in-rest-of-line-p)
       (akn/change-inner-quotes)
     (call-interactively (kmacro "i \" \" <left>")))
   (insert (key-description key-sequence))
   (call-interactively (kmacro "<right>"))))

(defun akn/open-config ()
  (interactive)
  (akn/switch-project (if-let* ((dotfiles (getenv "DOTFILES")))
                          (abbreviate-file-name dotfiles)
                        "~/prog/dotfiles")))
(defun akn/reload-config ()
  (interactive)
  (akn/load! "./config.el"))

(defun akn/+emacs-lisp-truncate-pin ()
  "Truncates long SHA1 hashes in `package!' :pin's.

This was built in to Doom but was removed:
https://github.com/doomemacs/doomemacs/commit/639fcc6a2e4dd3df9effef2da7b9d605aaf72214.

Use \\[visible-mode] to show the full hashes."
  (when (derived-mode-p 'emacs-lisp-mode)
    (save-excursion
      (goto-char (match-beginning 0))
      (and (stringp (plist-get (sexp-at-point) :pin))
           (search-forward ":pin" nil t)
           (let ((start (re-search-forward (rx ?\" (= 7 (not (any ?\n ?\"))))
                                           nil t))
                 (finish (and (re-search-forward "\"" (line-end-position) t)
                              (match-beginning 0))))
             (when (and start finish)
               (put-text-property start finish 'invisible 'akn/invisible-pin)
               (add-to-invisibility-spec '(akn/invisible-pin . t))
               (set-display-table-slot buffer-display-table 4 (string-to-vector (truncate-string-ellipsis))))))))
  nil)
(autoload 'truncate-string-ellipsis "mule-util")
(add-transient-hook! 'emacs-lisp-mode-hook
  (font-lock-add-keywords 'emacs-lisp-mode
                          `(("(package!\\_>" (0 (akn/+emacs-lisp-truncate-pin))))))

(use-package! elisp-indent-docstrings
  :ghook 'emacs-lisp-mode-hook
  :defer-incrementally t
  :config
  (after! so-long (add-to-list 'so-long-minor-modes 'elisp-indent-docstrings-mode)))

;;; undo

(after! undo-fu
  (setq
   ;; while undoing in a region, undo-fu is not supported; it just uses the regular emacs undo
   undo-fu-allow-undo-in-region t
   ;; TODO: figure out if I can get it to do this differently depending on context.
   ;; For now, evil-want-fine-undo seems like the safer setting.
   evil-want-fine-undo t))

;; Increase undo history limits
(after! (:or emacs undo-fu undo-tree)
  (let ((kb 1000))
    ;; NOTE: this config.el file is 285kb (as of 2024-08-10)
    ;;                                                              Defaults:
    ;;                                        doom if undo-tree |     doom  |    emacs
    (setq undo-limit        (* 8   256 kb)    ;           800kb |     256kb |    160kb
          undo-strong-limit (* 8  2000 kb)    ;        12,000kb |   2,000kb |    240kb
          undo-outer-limit  (* 8 36000 kb)))) ;       128,000kb |  36,000kb | 24,000kb

;;; lispy
(after! lispyville
  (setq lispyville-key-theme '((operators normal) ;remap evil-yank -delete -change -yank-line -delete-line -change-line -delete-char -delete-backward-char -substitute -change-whole-line -join
                               c-w                ;remap evil-delete-backward-word
                               c-u                ;remap evil-delete-back-to-indentation
                               (prettify insert)  ;remap evil-indent->lispyville-prettify
                               (atom-movement t)  ;remap WORD->atom
                               ;; slurp/barf-lispy ; > and <
                               slurp/barf-cp       ; > and <
                               text-objects
                               commentary
                               ;; normal state:
                               ;;    M-j=drag-forward  M-J=join M-s=splice M-r=raise-sexp M-t=transpose-sexps
                               ;;    M-k=drag-backward          M-S=split  M-R=raise-list M-v=convolute-sexp
                               additional
                               ;; insert state:
                               ;;    M-( = lispyville-wrap-round
                               ;;    M-[ = lispyville-wrap-brackets
                               ;;    M-{ = lispyville-wrap-braces
                               additional-insert))
  (lispyville-set-key-theme))

;; lispy-mode-map-special  +-./~_        <> 0123456789 ABCDEFGHIJKLMNOPQRS  VWX Z abcdefghjiklmnopqrstuvwxyz
;; lispy-mode-map-lispy    "();[]{}:^`#'@
;; lispy-mode-map-paredit  "();[]{}
;; lispy-mode-map-parinfer "();[]{}:^`#'
;; lispy-mode-map-evilcp   "();          <>                                                               y
;; lispy-mode-map-c-digits
;; lispy-mode-map-oleh

(when (modulep! :editor lispy)
  (setq lispy-key-theme
        '(special lispy parinfer c-digits))
  (when (featurep 'lispy)
    (lispy-set-key-theme lispy-key-theme)))

;;; Consult
(use-package! consult
  :defer-incrementally (compat pp tabulated-list text-property-search fringe bookmark)
  :config
  (after! consult-imenu
    (setf (alist-get 'emacs-lisp-mode consult-imenu-config)
          '(:toplevel "Functions"
            :types ((?f "Functions"   font-lock-function-name-face)
                    (?m "Macros"      font-lock-function-name-face)
                    (?p "Package"     font-lock-constant-face)
                    (?t "Types"       font-lock-type-face)
                    (?v "Variables"   font-lock-variable-name-face)
                    (?s "Section"     font-lock-doc-face)
                    (?a "Advice"      font-lock-function-name-face)
                    (?M "Minor modes" font-lock-constant-face)))))

  ;; preview
  (setq consult--customize-alist nil)

  (akn/advise-letf! consult-project-buffer (akn/a)
    (defadvice! akn/doom-project-root-a (args)
      :filter-args #'doom-project-root
      (if (eq (car args) 't)
          nil
        args))
    (consult-buffer-filter
     (cons (rx bos "*" (* anything) "*" eos)
           consult-buffer-filter)))

  (define-advice consult--temporary-files (:around (fn &rest args) akn/dont-preview-remote-files)
    (let ((ret (apply fn args)))
      (lambda (&optional name)
        (funcall ret
                 (if (and (stringp name)
                          (file-remote-p name))
                          ;; a connection is already established to the remote system it's on
                          ;; (not (file-remote-p name nil t)))
                     nil
                   name))))))

;;; dirvish peek mode

(use-package! dirvish-peek
  :when (modulep! :emacs dired)
  :defer-incrementally (dirvish find-func dired-loaddefs dnd dired cl-lib compat eieio edmacro format-spec pcase transient)
  :ghook 'doom-first-input-hook
  :config
  (advice-add #'dirvish--preview-update :around #'akn/save-selected-window-a)
  (akn/advise-letf! dirvish--find-file-temporarily (akn/a)
    ;; See `consult--filter-find-file-hook'
    (advice-add #'run-hooks :around #'consult--filter-find-file-hook))

  (dirvish-peek-mode))

(after! dirvish
  (pushnew! dirvish-preview-environment
            '(tab-line-exclude . t)
            '(tab-line-tabs-function . nil)))
(after! consult
  (pushnew! consult-preview-variables
            '(tab-line-exclude . t)
            '(tab-line-tabs-function . nil)))

;;; Projectile
(use-package! projectile
  :init
  ;; projectile-switch-project is usually the first thing I do, so I want it to be the first
  ;; thing loaded incrementally.
  (akn/doom-prioritize-load-packages-incrementally
   '(thingatpt
     seq ibuffer-loaddefs ibuffer
     ibuf-ext
     tool-bar ring ansi-color ansi-osc regexp-opt comint text-property-search compile
     grep
     lisp-mnt
     cl-generic project
     projectile))
  :config
  (defvar akn/projectile-timer nil)
  ;; discover projects in ~/prog and its sub-directories
  (setq! projectile-project-search-path '(("~/prog" . 2) ("~/prog/emacs/" . 2)))
  (when akn/projectile-timer (cancel-timer akn/projectile-timer))
  (defun akn/discover-projects ()
    (interactive)
    (shut-up
      (projectile-discover-projects-in-search-path)))
  (akn/after-idle! ((* 60 23) :repeat t :timer-name akn/projectile-timer)
    (akn/discover-projects))
  ;; SPC p D = +default/discover-projects
  ;; (projectile-discover-projects-in-search-path)
  ;; (defadvice! akn/discover-projects (_)
  ;;   :before #'projectile-switch-project
  ;;   (projectile-discover-projects-in-search-path)))

  ;; originally #'doom-project-find-file
  (setq! +workspaces-switch-project-function #'akn/workspace-switch-project)
  (defun akn/workspace-switch-project (&optional dirname)
    (interactive)
    (when (null dirname) (setq dirname default-directory))
    ;; only open dired if the project workspace is empty
    (when (null (+workspace-buffer-list))
      (dired dirname))
    (doom-project-find-file dirname))

  (akn/remove-from-list 'projectile-project-root-files-bottom-up ".project")
  (defvar akn/projectile-extra-markers (list ".project"))
  (define-advice projectile-root-marked (:override (dir &rest _) akn/a)
    (projectile-root-bottom-up dir (cons projectile-dirconfig-file akn/projectile-extra-markers))))

(defun akn/project-find-file (dir)
  "Use `doom-project-find-file' to find a file in the project root of DIR
(or DIR itself if not in a project).

Interactively, DIR is `default-directory'.

Also see `+default/find-file-under-here'."
  (interactive (list default-directory))
  (doom-project-find-file (or (doom-project-root dir)
                              dir)))
;; `projectile-find-file' seems to sometimes not be able to preview.
(map! [remap projectile-find-file] #'akn/project-find-file)

;;;; integrate project.el project list with projectile

;; Originally, I made this advice on `project--ensure-read-project-list', but
;; I'm not sure if that function is called enough that it'll cause performance
;; problems.
(defadvice! akn/project-add-projectile-projects-a (&rest _)
  :before #'project-prompt-project-dir
  :before #'project-known-project-roots
  :before #'project--read-project-list
  (with-demoted-errors "akn/project-add-projectile-projects-a: %S"
    (akn/letf! ((#'akn/project-add-projectile-projects-a #'ignore))
      (project--ensure-read-project-list)
      (when (and (boundp 'projectile-known-projects)
                 (listp projectile-known-projects)
                 (boundp 'project--list)
                 (listp project--list))
        (when-let* ((projects-to-add
                     (seq-difference projectile-known-projects (mapcar #'car project--list))))
          (dolist (proj projects-to-add)
            (when (stringp proj)
              (project--remember-dir proj t)))
          (project--write-project-list))))))

;;; PERF: speed up projectile over tramp

;; NOTE: these tramp projectile performance issues aren't as big with project.el

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
    ret))

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
(advice-add #'projectile-locate-dominating-file :around #'akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a)
(akn/advise-letf! project-try-vc (akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a)
  (advice-add #'locate-dominating-file :around #'akn/add-tramp-home-dir-to-locate-dominating-stop-dir-regexp-a))

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
  (setq tramp-default-method "rsync"
        remote-file-name-access-timeout 5
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
                                                               (seq (* nonl) "%" (any "rChp") (* nonl) "%" (any "rChp") (* nonl) "%" (any "rChp")))
                                                           nil t)))
               ;; "tramp-use-connection-share should also be set to nil or suppress if you use the ProxyCommand or ProxyJump options in your ssh configuration."
               (save-excursion (search-forward-regexp (rx bol (* blank) "Proxy") nil t)))))
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

;;; other tramp stuff

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


;;; embark
(use-package! embark
  :defer-incrementally (url-parse thingatpt compat ffap)
  :init
  (map! "s-;" #'embark-dwim)
  (setq embark-quit-after-action '((describe-symbol . t)
                                   (helpful-symbol . t)
                                   (t . nil)))
  :config
  (map! (:map embark-general-map
              "y" #'embark-copy-as-kill
              "s-c" #'embark-copy-as-kill))
  (setq! embark-cycle-key "C-;"))

;;; indent guides
(use-package! indent-bars
  :defer-incrementally (cl-lib map seq subr-x color timer outline font-lock face-remap cus-edit compat)
  :init
  (define-minor-mode akn/indent-guides-mode
    ""
    :group 'akn
    (if akn/indent-guides-mode
        (indent-bars-mode +1)
      (when (bound-and-true-p indent-bars-mode)
        (indent-bars-mode -1))))

  (add-hook! '+indent-guides-inhibit-functions
    (defun akn/indent-guides-mode-disabled-p ()
      (not akn/indent-guides-mode)))

  (defun akn/enable-indent-guides-maybe ()
    (unless (let ((akn/indent-guides-mode t))
              (run-hook-with-args-until-success '+indent-guides-inhibit-functions))
      (akn/indent-guides-mode)))

  (define-globalized-minor-mode akn/global-indent-guides-mode akn/indent-guides-mode
    akn/enable-indent-guides-maybe
    :predicate '(prog-mode text-mode conf-mode)
    :group 'akn))

;;; lsp

(setq! lsp-auto-guess-root t)

;;;; lsp minimal mode
;;  https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(define-minor-mode akn/lsp-minimal-mode
  ""
  :group 'akn
  :global nil
  (akn/mode-set akn/lsp-minimal-mode
    lsp-enable-symbol-highlighting nil
    lsp-ui-doc-enable nil
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse nil
    lsp-lens-enable nil
    lsp-headerline-breadcrumb-enable nil
    lsp-ui-sideline-enable nil
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-enable nil
    lsp-ui-sideline-show-hover nil
    lsp-modeline-code-actions-enable nil
    lsp-ui-sideline-enable nil
    lsp-ui-sideline-show-diagnostics nil
    lsp-eldoc-enable-hover nil
    lsp-modeline-diagnostics-enable nil
    lsp-signature-auto-activate nil
    lsp-signature-render-documentation nil
    lsp-completion-show-detail nil
    lsp-completion-show-kind nil
    lsp-ui-mode nil)
  (lsp-ui-mode (if lsp-ui-mode 1 -1)))

;;; sqlite
;; https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/
(add-to-list 'magic-mode-alist '("\\`SQLite format 3\x00" . akn/sqlite-view-file-magically))
(defun akn/sqlite-view-file-magically ()
  "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
  (when (and (require 'sqlite-mode nil 'noerror))
    ;;(y-or-n-p "Open with `sqlite-mode-open-file'? ")
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (sqlite-mode-open-file file-name))))

;;; automatically make #! script executable

(akn/remove-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; default is #o111 (ugo+x)
(setq! executable-chmod #o100) ; u+x
(setq! executable-prefix-env t)

;; TODO
(add-hook! 'after-save-hook #'akn/executable-make-buffer-file-user-executable-if-script-p)
(defun akn/executable-make-buffer-file-user-executable-if-script-p ()
  "Like `executable-make-buffer-file-executable-if-script-p', except
if it only makes it executable to the user (u+x) rather than
all (+x).

Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (>= (buffer-size) 2)
       (save-restriction
         (widen)
         (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
       ;; Eg file-modes can return nil (bug#9879).  It should not,
       ;; in this context, but we should handle it all the same.
       (with-demoted-errors "Unable to make file executable: %s"
         ;; (let* ((current-mode (file-modes (buffer-file-name)))
         ;;        (add-mode (logand ?\100 (default-file-modes))))
         ;;   (or (/= (logand ?\111 current-mode) 0)
         ;;       (zerop add-mode)
         ;;       (set-file-modes (buffer-file-name)
         ;;                       (logior current-mode add-mode)))))))
         (executable-chmod))))

;;; consult-omni

(defun akn/focus-this-frame ()
  (select-frame-set-input-focus (selected-frame)))
  ;; https://apple.stackexchange.com/a/276462
  ;; (when (featurep :system 'macos)
  ;;   (shut-up (start-process-shell-command "focus-this-frame"
  ;;                                         nil ;" *focus-this-frame*"
  ;;                                         (format "osascript -e 'tell application \"System Events\" to tell process \"%s\"' -e 'set frontmost to true' -e 'if windows is not {} then perform action \"AXRaise\" of item 1 of windows' -e 'end tell'"
  ;;                                                 (if (string-match-p "\\<nix\\>" (car (last load-path)))
  ;;                                                     "emacs"
  ;;                                                   "Emacs"))))))


;;; yasnippet
(use-package! yasnippet
  :config
  (map! ;; :i "S-SPC" yas-maybe-expand
   :i "M-TAB" (akn/cmds! (yas-maybe-expand-abbrev-key-filter #'yas-expand) #'yas-expand
                         (and (char-before) (string-match-p (rx alphanumeric) (char-to-string (char-before)))) #'yasnippet-capf
                         #'consult-yasnippet)))
   ;; (akn/defun akn/yasnippet-capf ()
   ;;   (interactive)
   ;;   (condition-case nil
   ;;       (progn (call-interactively #'yasnippet-capf)
   ;;              (akn/after-timer! (0) (call-interactively #'corfu-next)))
   ;;     (user-error (call-interactively #'consult-yasnippet))))))
   ;; (cmd! (let ((hippie-expand-try-functions-list '(yas-hippie-try-expand)))
   ;;         (call-interactively #'hippie-expand)))))
   ;; (akn/cmds! (yas-maybe-expand-abbrev-key-filter #'yas-expand) #'yas-expand
   ;;        #'yas-insert-snippet)))

(set-file-template! 'editorconfig-conf-mode :trigger "template")

;;; vertico
(add-hook 'vertico-mode-hook #'vertico-mouse-mode)
(use-package! vertico
  :defer-incrementally (compat)
  :config
  (map! :map vertico-map "DEL" (akn/cmds! (not (and (use-region-p) delete-selection-mode))
                                          #'vertico-directory-delete-char))

  (cl-defmethod vertico--setup :extra "akn/vertico--setup2" :after (&context (vertico-mouse-mode (eql t)))
    (run-hooks 'akn/vertico-buffer-hook))

  (add-hook 'akn/vertico-buffer-hook #'akn/local-smooth-scroll-disabled-mode))

;;; show line numbers when narrowing
(map! "C-x n e" #'widen    ; e for "expand"
      :n "z w"  #'widen)   ; to match "z n"

(defvar-local akn/old-display-line-numbers t)
(defadvice! akn/line-numbers-on-when-narrowing-a (&rest _)
  :after #'narrow-to-region
  (when (and akn/old-display-line-numbers
             (symbolp this-command)
             (string-match-p "narrow" (symbol-name this-command)))
    (setq-local akn/old-display-line-numbers display-line-numbers)
    (setq-local doom--line-number-style t)
    (setq-local display-line-numbers t)))
(defadvice! akn/line-numbers-off-when-widening-a (&rest _)
  :after #'widen
  (when (and (not akn/old-display-line-numbers)
             (symbolp this-command)
             (string-match-p "\\(?:narrow\\|widen\\)" (symbol-name this-command)))
    (setq-local akn/old-display-line-numbers t)
    (setq-local doom--line-number-style nil)
    (setq-local display-line-numbers nil)))

;;; Markdown and Outline Heading Faces


(after! markdown-mode
  (map! :map markdown-mode-map
        [remap markdown-enter-key] #'akn/markdown-enter-key
        :gie "S-RET" (akn/defun akn/markdown-shift-enter ()
                       (interactive)
                       (let ((bullet (cl-fifth (markdown-cur-list-item-bounds))))
                         (newline)
                         (markdown-indent-line)
                         (dotimes (_ (length bullet))
                           (insert " "))))
        :gie "S-<return>" #'akn/markdown-shift-enter
        "s-i" #'markdown-insert-italic
        "s-b" #'markdown-insert-bold)
  ;; In markdown, if we're in a list, add a new bullet when I press enter
  (setq! markdown-indent-on-enter 'indent-and-new-item)
  (defun akn/markdown-enter-key ()
    (interactive)
    (cond
     ;; Table
     ((markdown-table-at-point-p)
      (call-interactively #'markdown-table-next-row))
     ;; Indent non-table text
     (markdown-indent-on-enter
      (let (bounds)
        (if (and (memq markdown-indent-on-enter '(indent-and-new-item))
                 (setq bounds (markdown-cur-list-item-bounds)))
            (let ((beg (cl-first bounds))
                  (end (cl-second bounds))
                  (nonlist-indent (cl-fourth bounds))
                  (checkbox (cl-sixth bounds)))
              ;; Point is in a list item
              (if (= (- end beg) (+ nonlist-indent (length checkbox)))
                  ;; Delete blank list
                  (if (and (memq (save-excursion (beginning-of-line) (char-after)) '(?\s ?\t))
                           (fboundp 'evil-markdown-shift-left))
                      (evil-markdown-shift-left-line)
                    (delete-region beg end))
                   ;; (newline)
                   ;; (markdown-indent-line))
                (call-interactively #'markdown-insert-list-item)))
          ;; Point is not in a list
          (newline)
          (markdown-indent-line))))
     ;; Insert a raw newline
     (t (newline))))

  ;; markdown-mode doesn't deal with tabs right
  (defadvice! akn/list-item-tabs-a (&rest _)
    :after #'markdown-insert-list-item
    (when indent-tabs-mode
      (save-excursion
        (save-restriction
          (narrow-to-region (line-beginning-position) (line-end-position))
          (beginning-of-line)
          (when-let* ((search (search-forward-regexp (rx bol (+ " ")) nil t))
                      (num-spaces (length (without-restriction (match-string 0)))))
            (without-restriction
              (replace-match (make-string num-spaces ?\t))))))))

  (after! evil-markdown
    (map! :map evil-markdown-mode-map
          :i "<tab>"     (akn/cmds! (not (markdown-table-at-point-p)) #'evil-markdown-shift-right-line)
          :i "TAB"       (akn/cmds! (not (markdown-table-at-point-p)) #'evil-markdown-shift-right-line)
          :i "<backtab>" (akn/cmds! (not (markdown-table-at-point-p)) #'evil-markdown-shift-left-line)
          :i "S-<tab>"   (akn/cmds! (not (markdown-table-at-point-p)) #'evil-markdown-shift-left-line)))


  ;; syntax highlight code blocks
  (setq! markdown-fontify-code-blocks-natively t))

;;; tty
;; https://stackoverflow.com/a/62266648
;; Mouse scrolling in terminal emacs
(add-hook 'tty-setup-hook #'akn/terminal-mouse-scrolling-h)
(defun akn/terminal-mouse-scrolling-h ()
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; maybe https://github.com/doomemacs/doomemacs/issues/4137#issuecomment-836264301 ?

;;; treesit
(after! treesit
  (unless (eq treesit-font-lock-level 4)
    (setq! treesit-font-lock-level 4))
  (defadvice! akn/treesit-explore-mode-dont-ask-if-one-choice-a (fn &rest outer-args)
    :around #'treesit-explore-mode
    (let* ((akn/completing-read-function--old completing-read-function)
           (completing-read-function (lambda (prompt collection &rest args)
                                       (if (and (null current-prefix-arg) (sequencep collection) (length= collection 1))
                                           (format "%s" (car collection))
                                         (apply akn/completing-read-function--old prompt collection args)))))
      (apply fn outer-args)))

  (map! :map treesit--explorer-tree-mode-map
        :mn "q" #'quit-window))

;;; hercules
;; https://github.com/cyruseuros/hercules

(use-package! hercules
  ;; :demand t
  :config
  (add-hook! 'doom-escape-hook :depth 90
    (defun akn/quit-hercules ()
      (hercules--hide)))

  ;; (after! ibuffer
  ;;   (hercules-def
  ;;    :toggle-funs #'ibuffer-mode
  ;;    :keymap 'ibuffer-mode-map
  ;;    :hide-funs '(read-from-minibuffer keyboard-quit))))
  (after! macrostep
    (hercules-def
     :toggle-funs #'macrostep-mode
     :keymap 'macrostep-keymap
     :hide-funs '(read-from-minibuffer keyboard-quit))))

;;; copy-as-format
(use-package! copy-as-format
  :init
  ;; inspired by https://develop.spacemacs.org/layers/+misc/copy-as-format/README.html
  (map!
   (:leader
    (:prefix ("c y" . "copy-as-format")
     "y" #'copy-as-format
     "a" #'copy-as-format-asciidoc
     "b" #'copy-as-format-bitbucket
     "c" #'copy-as-format-hipchat
     "d" #'copy-as-format-disqus
     "g" #'copy-as-format-github
     "h" #'copy-as-format-html
     "j" #'copy-as-format-jira
     "l" #'copy-as-format-gitlab
     "m" #'copy-as-format-markdown
     "o" #'copy-as-format-org-mode
     "p" #'copy-as-format-pod
     "r" #'copy-as-format-rst
     "s" #'copy-as-format-slack
     "w" #'copy-as-format-mediawiki)))
  :config
  (setq! copy-as-format-default "github"))

;;; PERF: speed up killing emacs

(after! projectile
  (akn/advise-letf! projectile-ignored-projects (akn/fast-a)
    (defadvice! akn/projectile-ignored-projects--faster-file-truename-a (fn filename &rest rest)
      :around #'file-truename
      (let ((filename2
             (if (and (stringp filename)
                      (string-match-p (rx bos "~" (or "/" eos)) filename))
                 (concat (getenv-internal "HOME") (substring-no-properties filename 1))
               filename)))
        (if (or rest
                (not (stringp filename2))
                (string-match-p (rx (or ".doom.d" ".config/doom" "straight")) filename2) ; because these might be symlinks
                (not (string-match-p (rx bos "/") filename2)))
            (progn ;; (message "running for %s (%s, %s, %s, %s)" filename rest (stringp filename2) (string-match-p (rx bos "/") filename2) (string-match-p (rx (or ".config/doom" "straight")) filename2))
              (apply fn filename rest))
          filename2)))))

(after! elgrep
  (remove-hook 'kill-emacs-hook #'elgrep-save-elgrep-data-file))

;;; spelling
(after! ispell
  (setq! ispell-personal-dictionary (akn/expand-file "~/.config/enchant/en.dic")))
(when (modulep! :checkers spell)
  (remove-hook! '(text-mode-hook org-mode-hook markdown-mode-hook TeX-mode-hook rst-mode-hook mu4e-compose-mode-hook message-mode-hook git-commit-mode-hook)
    #'spell-fu-mode
    #'flyspell-mode))

;;; garbage collect on frame unfocus
;; https://news.ycombinator.com/item?id=39190110
(add-hook 'akn/emacs-focus-out-hook #'garbage-collect)

;;; preview while asking whether to save buffers (in particular, when killing emacs)

(akn/advise-letf! save-some-buffers (akn/preview-a)
  (define-advice map-y-or-n-p (:filter-args (args) akn/preview-a)
    (when (functionp (car args))
        (setf (car args)
              (let ((old-fun (car args)))
                (lambda (buffer)
                  (display-buffer buffer '((display-buffer-reuse-window) . ()))
                  (funcall old-fun buffer)))))
    args))
(define-advice save-some-buffers (:around (fn &rest args) akn/preview-a2)
  (save-window-excursion
    (apply fn args)))

;;; keypad (extracted from meow)
(use-package! akn-keypad
  :defer t
  :init
  (map! :mn "C-SPC" #'akn-keypad)
  :config
  (akn-keypad--setup-which-key nil))

;;; so-long

(after! so-long
  (pushnew! so-long-minor-modes
            'parinfer-rust-mode
            'outline-minor-mode
            'outli-mode
            'display-line-numbers-mode
            'flycheck-popup-tip-mode
            'page-break-lines-mode
            '+word-wrap-mode
            'adaptive-wrap-prefix-mode
            'goto-address-mode
            'goto-address-prog-mode
            'lsp-mode
            'eglot-mode)
  (pushnew! so-long-variable-overrides
            '(display-line-numbers . nil)
            '(display-line-numbers-type . nil)))

;;; hardtime

;; https://github.com/ichernyshovvv/hardtime.el
;; https://github.com/m4xshen/hardtime.nvim
(use-package! hardtime
  :defer-incrementally t
  :config
  (defun akn/hardtime-check-command ()
    "Return non-nil if the currently executed command should be checked."
    (unless (akn/class-based-on-time)
      (memq this-command '(next-line previous-line evil-previous-visual-line
                           right-char left-char left-word right-word
                           forward-paragraph backward-paragraph
                           evil-forward-char evil-backward-char
                           akn/evil-forward-char-cross-lines akn/evil-backward-char-cross-lines
                           evil-next-line evil-previous-line
                           evil-next-visual-line evil-previous-visual-line))))
  (hardtime-mode)
  :custom
  (hardtime-predicate #'akn/hardtime-check-command))

;;; auto-fill

(after! simple
  ;; make pressing period do auto-fill
  (set-char-table-range auto-fill-chars ?\. t))

;;; word count

(defun akn/count-paragraphs (start end)
  "Return number of paragraphs between START and END.

From https://emacs.stackexchange.com/a/45586."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (- (buffer-size) (forward-paragraph (buffer-size))))))

(define-advice count-words--format (:override (str start end) akn/add-paragraphs-a)
  (let ((paragraphs (akn/count-paragraphs start end))
        (lines (count-lines start end))
        (sentences (count-sentences start end))
        (words (count-words start end))
        (chars (- end start)))
    (format "%s has %d paragraph%s, %d line%s, %d sentence%s, %d word%s, and %d character%s"
            str
            paragraphs (if (= paragraphs 1) "" "s")
            lines (if (= lines 1) "" "s")
            sentences (if (= sentences 1) "" "s")
            words (if (= words 1) "" "s")
            chars (if (= chars 1) "" "s"))))

;;; vundo

(after! vundo
  (defvar-local akn/vundo-saved-state nil)
  (add-hook! 'vundo-pre-enter-hook
    (defun akn/vundo-enter-h ()
      (setq-local akn/vundo-saved-state
                  (buffer-local-set-state inhibit-modification-hooks t
                                          parinfer-rust--disable t))))
  (add-hook! 'vundo-post-exit-hook
    (defun akn/vundo-exit-h ()
      (buffer-local-restore-state akn/vundo-saved-state)
      (setq-local akn/vundo-saved-state nil))))

;;; fish-completion-mode

(use-package! fish-completion
  :after-call (eshell-mode-hook shell-mode-hook eat-mode-hook mistty-mode-hook term-mode-hook)
  ;; TODO: PR to emacs-fish-completion: autoload
  :commands (global-fish-completion-mode)
  :config
  ;; The original implementation seems to be wrong in multiple ways.
  ;; TODO: PR to emacs-fish-completion
  (define-minor-mode fish-completion-mode
    "Turn on/off fish shell completion in all future shells or Eshells.

In `shell', completion is replaced by fish completion.
In `eshell', fish completion is only used when `pcomplete' fails."
    :init-value nil
    (if fish-completion-mode
        (unless (or fish-completion-inhibit-missing-fish-command-warning
                    fish-completion-command)
          (warn "Fish is not installed. fish-completion-mode will silently fall back to Bash for completions.")))
    (akn/mode-set fish-completion-mode
      pcomplete-default-completion-function #'fish-completion-shell-complete))

  (global-fish-completion-mode))

;;; dtrt-indent

(after! dtrt-indent
  (setq dtrt-indent-min-relevant-lines 1))

;;; cycle-spacing

(setq cycle-spacing-actions '( just-one-space     delete-all-space    (delete-space-after 0) (delete-space-before 0) restore
                              (just-one-space -) (delete-all-space -) (delete-space-after -) (delete-space-before -) restore))
(define-advice cycle-spacing (:after (&rest _) akn/print-action-a)
  (when (called-interactively-p 'any)
    (message "%s" (plist-get cycle-spacing--context :last-action))))
(define-advice cycle-spacing (:around (fn &rest args) akn/no-repeats-a)
  (apply fn args)
  ;; with multiple cursors, we want behavior that's the same for all cursors
  (unless (or (bound-and-true-p multiple-cursors-mode) (bound-and-true-p evil-mc-mode))
    (let ((num-times 1)
          (last-command #'cycle-spacing))
      (while (and (< num-times (1+ (length cycle-spacing-actions)))
                  (not (eq (plist-get cycle-spacing--context :last-action) 'restore))
                  (equal (akn/space-around-point) (plist-get cycle-spacing--context :whitespace-string)))
        (message "%s" (length (akn/space-around-point)))
        (apply fn args)
        (cl-incf num-times)))))
(defun akn/space-around-point ()
  "From `cycle-spacing'."
  (let ((orig-pos (point))
        (skip-characters " \t\n\r"))
    (save-excursion
      (skip-chars-backward skip-characters)
      (constrain-to-field nil orig-pos)
      (let ((start (point))
            (end   (progn
                     (skip-chars-forward skip-characters)
                     (constrain-to-field nil orig-pos t))))
        (buffer-substring start end)))))

;;; buffer names
(setq! uniquify-min-dir-content 1
       uniquify-trailing-separator-p 'forward)

;;; polymode
(after! polymode
  (define-advice doom-temp-buffer-p (:after-while (buf) akn/polymode-a)
    (not (akn/polymode-p buf)))

  (dolist (fn (list #'+tabs-name-fn
                    #'+tabs-fn
                    #'tab-line-format
                    #'tab-line-format-template))
    (advice-add fn :around #'polymode-with-current-base-buffer))

  (define-advice python-shell-send-buffer (:around (fn &optional send-main msg &rest args) akn/a)
    (if (akn/polymode-p)
        (python-shell-send-region (pm-execute-narrowed-to-span #'point-min) (pm-execute-narrowed-to-span #'point-max) send-main msg)
      ;; widens
      (apply fn send-main msg args)))

  (setq-hook! 'python-mode-hook
    polymode-eval-region-function
    (lambda (beg end &optional _msg)
      (python-shell-send-region beg end nil nil)))

  (dolist (fn (list #'python-shell-send-defun
                    #'python-shell-send-statement))
    (advice-add fn :around #'pm-execute-narrowed-to-span))

  ;; adaptive-wrap seems to negatively impact performance when switching between inner chunks (i.e., between buffers)
  (add-hook! '(polymode-init-host-hook polymode-init-inner-hook)
    (+word-wrap-mode -1))

  (pushnew! polymode-move-these-minor-modes-from-old-buffer
            'tab-line-mode
            'better-backup-buffer-mode)

  (add-hook! 'markdown-mode-hook
    (defun akn/disable-code-block-fontify-if-polymode-h ()
      "Disables `markdown-fontify-code-blocks-natively' in polymode buffers.

A polymode buffer will handle fontification of code blocks itself, so
there's no need for `markdown-mode' to reduplicate the effort."
      (when (akn/polymode-p)
        (setq-local markdown-fontify-code-blocks-natively nil)))))

;;; remember.el
;; https://baty.net/posts/2025/03/i-went-a-little-nuts-with-remember-mode-in-emacs/
;; https://emacs-fu.blogspot.com/2009/04/remember.html
;; https://www.gnu.org/software/emacs/manual/html_mono/org.html#From-Carsten-1
;; should probably just use org-capture

(after! remember
  (setq! remember-all-handler-functions t
         remember-handler-functions '(remember-append-to-file remember-store-in-files remember-diary-extract-entries)
         remember-data-directory (expand-file-name "remember" doom-data-dir)
         remember-data-file (expand-file-name "remember.txt" doom-data-dir))

  (advice-add #'remember-append-to-file :around #'akn/inhibit-read-only-a)
  (advice-add #'remember-store-in-files :around #'akn/inhibit-read-only-a)
  (advice-add #'remember-diary-extract-entries :around #'akn/inhibit-read-only-a)
  (advice-add #'remember-store-in-mailbox :around #'akn/inhibit-read-only-a))

;;; calendar

(setq evil-collection-calendar-want-org-bindings t)

;;;; diary

(after! calendar
  (setq! diary-file (expand-file-name "diary.txt" "~/org")))

;;;; holidays

(setq holiday-other-holidays
      '((holiday-sexp
         '(if (zerop (% year 4))
              (calendar-gregorian-from-absolute
               (1+ (calendar-dayname-on-or-before
                       1 (+ 6 (calendar-absolute-from-gregorian
                                 (list 11 1 year)))))))
         "US Presidential Election")))

;; because `calendar-holidays' is initialized to include `holiday-other-holidays'
(when (featurep 'holidays)
  (makunbound 'calendar-holidays)
  (load "holidays" nil 'nomessage))

;;; akn/terminal-quit-mode

;; TODO: combine with `akn/info-standalone'
(define-minor-mode akn/terminal-quit-mode
  "Press q to quit"
    :group 'akn
  :keymap (make-sparse-keymap))
(map! (:map akn/terminal-quit-mode-map
       :mnvg "q" (akn/cmds! (not (display-graphic-p)) #'save-buffers-kill-terminal))
      (:after magit
       :map magit-mode-map
       :mnvg "q" (akn/cmds! (and akn/terminal-quit-mode (not (display-graphic-p))) #'save-buffers-kill-terminal
                            #'+magit/quit))
      (:after dired
       :map dired-mode-map
       :mnvg "q" (akn/cmds! (and akn/terminal-quit-mode (not (display-graphic-p))) #'save-buffers-kill-terminal
                            #'+dired/quit-all)))

;;; opening links

(defun akn/open-links (region-begin region-end)
  "Open all the links in region or paragraph."
  (interactive
   (if (or (not transient-mark-mode) (use-region-p))
       (list (region-beginning) (region-end))
     (save-excursion
       (forward-paragraph)
       (list (save-excursion (backward-paragraph) (point))
             (point)))))
  (save-restriction
    (narrow-to-region region-begin region-end)
    (link-hint-open-all-links)))

;;; doom check for updates

;; TODO: add to modeline
(defun akn/doom-check-for-updates ()
  (interactive)
  ;; (akn/run-command "doom check-for-updates"
  ;;                  :connection-type '(pty . pipe)
  ;;                  :name "akn/doom-check-for-updates"
  ;;                  :on-output (lambda (text) (message "%s" (string-trim text)))
  ;;                  :boring t))
  (akn/get-doom-up-to-date-p
   (lambda (up-to-date-p)
     (message "%s" (or up-to-date-p "update available!")))))

;;; doom/reload

(define-advice doom/reload (:before (&rest _) akn/no-error-require-a)
  (mapc (lambda (pkg) (ignore-errors (require pkg nil 'noerror))) (cdr doom-incremental-packages))
  (setf (cdr doom-incremental-packages) nil))

;;; find-file: open dired after finding nonexisting directory

;; `doom-create-missing-directories-h' works great. But suppose the file you're
;; finding is *itself* a nonexisting directory - for example,
;; (find-file "/tmp/i-dont-exist/") instead of (find-file "/tmp/i-dont-exist/file").
;; The directory will be created, but then you're put into a useless, empty
;; buffer. Instead, I want to be put into a dired buffer.

(add-hook! 'find-file-hook :depth -25
  (defun akn/no-longer-nonexisting-directory-h ()
    (when-let* (((and buffer-file-truename
                      find-file-run-dired
                      (file-directory-p buffer-file-truename)))
                (buf (run-hook-with-args-until-success
                      'find-directory-functions
                      (if find-file-visit-truename
                          (abbreviate-file-name buffer-file-truename)
                        buffer-file-name))))
      (kill-buffer (current-buffer))
      (set-buffer buf))))

;;; magic-mode-alist

;; might speed it up slightly
(akn/remove-from-list 'magic-mode-alist '("#compdef " . sh-mode))
(add-to-list          'magic-mode-alist '("\\`#compdef " . sh-mode))

;;; ediff

(after! ediff
  (define-minor-mode akn/ediff-buffer-mode
    "A mode automatically enabled in ediff buffers."
    :group 'akn
    :interactive nil)

  (add-hook 'ediff-prepare-buffer-hook #'akn/ediff-buffer-mode)
  (add-hook! '(ediff-startup-hook ediff-cleanup-hook ediff-quit-hook ediff-quit-merge-hook ediff-suspend-hook
               ediff-prepare-buffer-hook ediff-after-setup-windows-hook
               ediff-after-session-group-setup-hook ediff-quit-session-group-hook)
    (defun akn--enable-or-disable-ediff-buffer-mode-h ()
      (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C ediff-ancestor-buffer))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (if ediff-this-buffer-ediff-sessions
                (unless akn/ediff-buffer-mode (akn/ediff-buffer-mode))
              (when akn/ediff-buffer-mode (akn/ediff-buffer-mode -1))))))))

  (add-hook! 'akn/ediff-buffer-mode-hook
    (defun akn--ediff-disable-hl-line-h ()
      (akn/mode-set akn/ediff-buffer-mode
        hl-line-mode nil
        pulsar-mode nil)
      (hl-line-mode (if hl-line-mode 1 -1))
      (pulsar-mode (if pulsar-mode 1 -1)))))

;;; file-local variables

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
