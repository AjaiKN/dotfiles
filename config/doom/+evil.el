;;; +evil.el -*- lexical-binding: t; -*-

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


;;; Evil
(setq!
 ;; In vim, v$ will highlight the newline at the end of the line too.
 ;; That doesn't really make sense to me.
 evil-v$-excludes-newline t
 ;; behavior is reversed: now use /g to make it substitute just the first occurence on each line
 evil-ex-substitute-global t)

;; (setq! evil-search-module 'isearch)
(setq! evil-search-module 'evil-search)
(use-package! evil-easymotion
  :commands (evilem-motion-ex-search-next
             evilem-motion-ex-search-previous
             evilem-motion-ex-search-word-forward
             evilem-motion-ex-search-word-backward)
  :init
  (map!
   [remap evilem-motion-search-next]          (akn/cmds! (eq evil-search-module 'evil-search) #'evilem-motion-ex-search-next          #'evilem-motion-search-next)
   [remap evilem-motion-search-previous]      (akn/cmds! (eq evil-search-module 'evil-search) #'evilem-motion-ex-search-previous      #'evilem-motion-search-previous)
   [remap evilem-motion-search-word-forward]  (akn/cmds! (eq evil-search-module 'evil-search) #'evilem-motion-ex-search-word-forward  #'evilem-motion-search-word-forward)
   [remap evilem-motion-search-word-backward] (akn/cmds! (eq evil-search-module 'evil-search) #'evilem-motion-ex-search-word-backward #'evilem-motion-search-word-backward))
  :config
  ;; from doom
  (evilem-make-motion evilem-motion-ex-search-next          #'evil-ex-search-next          :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-ex-search-previous      #'evil-ex-search-previous      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-ex-search-word-forward  #'evil-ex-search-word-forward  :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-ex-search-word-backward #'evil-ex-search-word-backward :bind ((evil-ex-search-highlight-all nil)))
  ;; from evil-easymotion
  (evilem-make-motion evilem-motion-search-next             #'evil-search-next             :bind (((symbol-function #'isearch-lazy-highlight-update) #'ignore) (search-highlight nil)))
  (evilem-make-motion evilem-motion-search-previous         #'evil-search-previous         :bind (((symbol-function #'isearch-lazy-highlight-update) #'ignore) (search-highlight nil)))
  (evilem-make-motion evilem-motion-search-word-forward     #'evil-search-word-forward     :bind (((symbol-function #'isearch-lazy-highlight-update) #'ignore) (search-highlight nil)))
  (evilem-make-motion evilem-motion-search-word-backward    #'evil-search-word-backward    :bind (((symbol-function #'isearch-lazy-highlight-update) #'ignore) (search-highlight nil))))

;; so I can change `evil-search-module' buffer-locally, e.g. in vlf files
(map!
 [remap evil-search-forward]                    (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-forward                 (eq evil-search-module 'isearch) #'evil-search-forward)
 [remap evil-search-backward]                   (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-backward                (eq evil-search-module 'isearch) #'evil-search-backward)
 [remap evil-search-word-forward]               (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-word-forward            (eq evil-search-module 'isearch) #'evil-search-word-forward)
 [remap evil-search-word-backward]              (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-word-backward           (eq evil-search-module 'isearch) #'evil-search-word-backward)
 [remap evil-search-unbounded-word-forward]     (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-unbounded-word-forward  (eq evil-search-module 'isearch) #'evil-search-unbounded-word-forward)
 [remap evil-search-unbounded-word-backward]    (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-unbounded-word-backward (eq evil-search-module 'isearch) #'evil-search-unbounded-word-backward)
 [remap evil-search-next]                       (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-next                    (eq evil-search-module 'isearch) #'evil-search-next)
 [remap evil-search-previous]                   (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-previous                (eq evil-search-module 'isearch) #'evil-search-previous)
 [remap evil-ex-search-forward]                 (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-forward                 (eq evil-search-module 'isearch) #'evil-search-forward)
 [remap evil-ex-search-backward]                (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-backward                (eq evil-search-module 'isearch) #'evil-search-backward)
 [remap evil-ex-search-word-forward]            (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-word-forward            (eq evil-search-module 'isearch) #'evil-search-word-forward)
 [remap evil-ex-search-word-backward]           (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-word-backward           (eq evil-search-module 'isearch) #'evil-search-word-backward)
 [remap evil-ex-search-unbounded-word-forward]  (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-unbounded-word-forward  (eq evil-search-module 'isearch) #'evil-search-unbounded-word-forward)
 [remap evil-ex-search-unbounded-word-backward] (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-unbounded-word-backward (eq evil-search-module 'isearch) #'evil-search-unbounded-word-backward)
 [remap evil-ex-search-next]                    (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-next                    (eq evil-search-module 'isearch) #'evil-search-next)
 [remap evil-ex-search-previous]                (akn/cmds! (eq evil-search-module 'evil-search) #'evil-ex-search-previous                (eq evil-search-module 'isearch) #'evil-search-previous))

;; still use doom's emacs cursor color, but make it a bar instead of a box
(setq evil-emacs-state-cursor `(bar ,#'+evil-emacs-cursor-fn))

(setq evil-motion-state-cursor evil-normal-state-cursor)

(map! :m "q" #'quit-window)

(after! markdown-mode
  (defcustom akn/evil-want-o-continue-bullets t
    ""
    :type '(boolean)
    :group 'akn)
  (define-advice akn/o-continue-bullet (:after (count) akn/o-continue-bullet)
    (when (and (derived-mode-p 'markdown-mode)
               (eq this-command #'evil-open-below)
               akn/evil-want-o-continue-bullets
               (eq count 1))
      (let ((prev-line (save-excursion (forward-line -1) (thing-at-point 'line)))
            (this-line (thing-at-point ' line)))
        (when (and (string-match (rx bol
                                     (group (* whitespace))
                                     (group (any "-*+") (* whitespace)))
                                 prev-line)
                   (string-match-p (rx bol (literal (match-string-no-properties 1 prev-line)))
                                   this-line))
          (insert (match-string-no-properties 2 prev-line)))))))

;;Why can I select then paste if delete-selection-mode is off?
;;ANSWER: Without delete-selection-mode, you can't do that in Emacs state, only insert state.

(delete-selection-mode)
(remove-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
(remove-hook 'evil-insert-state-exit-hook  #'+default-disable-delete-selection-mode-h)

;; (defconst akn/delete-selection-things
;;   (cl-loop for sym being the symbols
;;            when (fboundp sym)
;;            when (get sym 'delete-selection)
;;            collect (cons sym (get sym 'delete-selection)))
;;   "See `delete-selection-helper', `delete-selection-pre-hook', and
;; `delete-selection-mode'.")

;;TODO: https://emacs.stackexchange.com/a/32350 \"0p
;; (evil-define-operator akn/evil-change+register-c
;;   (&rest args)
;;   "Like EVIL-CHANGE, but put the thing copied *second* on the kill ring
;; instead of first."
;;   (interactive "<R><x><y>")
;;   (let ((ret (apply #'evil-change args)))
;;     ret))
(defadvice! akn/swap-first-and-second-kill-ring-a (&rest _args)
  "Swap the first and second elements of the kill-ring.
So to get the string deleted after a change, use M-y or C-p."
  :after #'evil-change
  (when (and kill-ring (cdr kill-ring))
    (setq kill-ring (cons (cadr kill-ring)
                          (cons (car kill-ring)
                                (cddr kill-ring))))
    (setq kill-ring-yank-pointer kill-ring)))
;; (undefadvice! akn/evil-change+register-c (&rest _args)
;;   "Put the thing deleted into the ?c register instead of copying it immediately."
;;   :before #'akn/evil-change
;;   (when (not evil-this-register)
;;     (message "changing")
;;     (evil-use-register ?c)))
;; (undefadvice! akn/evil-change+register-c (beg end &optional type _register yank-handler delete-func)
;;   "Put the thing deleted into the ?c register instead of copying it immediately."
;;   :filter-args #'evil-change
;;   (list beg end type ?c yank-handler delete-func))
;; (undefadvice! akn/evil-change+after (&rest _args)
;;   :after #'evil-change
;;   (message "done changing"))

(setq! evil-collection-setup-minibuffer t)
(when (boundp '+default-minibuffer-maps)
  (map! :map ,+default-minibuffer-maps
        ;; jk (from `evil-escape-mode') should still work to go to normal state.
        :i "<escape>" #'abort-recursive-edit
        :i "C-g" #'abort-recursive-edit))

;; By default, when you press "q" to record a macro, there's nothing telling
;; you that you're about to record a macro into the key you type next.
(advice-add 'evil-record-macro :before #'akn/evil-record-macro-before)
(defun akn/evil-record-macro-before (_register)
  ;; this is exactly the same as the interactive section in evil-record-macro,
  ;; except we provide an argument to evil-read-key
  (interactive
   (list
    (unless (and evil-this-macro defining-kbd-macro)
      (or evil-this-register (evil-read-key "Record macro in register (type key):"))))))

;; When pressing <escape> from insert state, if the region is active, switch
;; to visual state instead of normal state.
(map! :map evil-insert-state-map
      [remap evil-normal-state] (akn/cmds! (and (use-region-p)
                                                (not (or (bound-and-true-p multiple-cursors-mode) (bound-and-true-p rectangular-region-mode))))
                                           #'akn/evil-visual-state-from-insert
                                           ;; e.g., if the region is empty
                                           (and (region-active-p)
                                                (not (or (bound-and-true-p multiple-cursors-mode) (bound-and-true-p rectangular-region-mode))))
                                           (cmd! (deactivate-mark) (evil-normal-state))))
(defun akn/evil-visual-state-from-insert ()
  (interactive)
  (let ((was-rectangle (bound-and-true-p rectangle-mark-mode)))
    (when (and (eq (region-end) (point)) evil-move-cursor-back)
      (evil-backward-char nil nil t))
    (if was-rectangle
        (progn
          (evil-visual-state 1)
          (setq evil-visual-selection 'block))
      (evil-visual-state 1))
    (setq evil-previous-state 'normal)))
    ;; (call-interactively #'evil-visual-block)))

;; Make "i" and "a" from visual state switch to insert state (instead of the
;; regular selecting of "inner" and "around" text objects), unless nothing has
;; been selected yet.
(map! :map evil-visual-state-map
      "I" (akn/cmds! (memq (evil-visual-type) '(block line))
                     #'evil-mc-make-cursor-in-visual-selection-beg
                     #'akn/evil-visual-insert)
      "z I" #'akn/evil-visual-insert
      "A" (akn/cmds! (memq (evil-visual-type) '(block line))
                     #'evil-mc-make-cursor-in-visual-selection-end
                     #'akn/evil-visual-append)
      "z A" #'akn/evil-visual-append

      ;; similarly to rectangle-mark-mode's `rectangle-exchange-point-and-mark'
      ;; (except in a different order)
      "C-x C-x" (cmds! (eq (evil-visual-type) 'block)
                       #'evil-visual-rotate))
(defun akn/evil-visual-insert ()
  (interactive)
  (let ((was-visual-block (eq (evil-visual-type) 'block)))
    (when was-visual-block
      (message "setting")
      (evil-visual-char))
    (call-interactively #'evil-insert)
    (setq-local transient-mark-mode t)
    (when was-visual-block
      (call-interactively #'rectangle-mark-mode))))
(defun akn/evil-visual-append ()
  (interactive)
  (let ((was-visual-block (eq (evil-visual-type) 'block)))
    (when was-visual-block
      (message "setting")
      (evil-visual-char))
    (call-interactively #'evil-append)
    (setq-local transient-mark-mode t)
    (when was-visual-block
      (call-interactively #'rectangle-mark-mode))))

;;; evil-quickscope
(use-package! evil-quickscope
  :after-call doom-first-input-hook
  :config
  ;; I don't want evil-quickscope to highlight the second instance at all, just the first
  (custom-set-faces! '(evil-quickscope-second-face :underline unspecified :inherit unspecified))

  (map! :map evil-quickscope-mode-map
        ;; I think remapping might be more reliable
        :m "f" nil
        :m "F" nil
        :m "t" nil
        :m "T" nil
        [remap evil-find-char]             #'evil-quickscope-find-char
        [remap evil-find-char-backward]    #'evil-quickscope-find-char-backward
        [remap evil-find-char-to]          #'evil-quickscope-find-char-to
        [remap evil-find-char-to-backward] #'evil-quickscope-find-char-to-backward
        [remap evil-snipe-f]               #'evil-quickscope-find-char
        [remap evil-snipe-F]               #'evil-quickscope-find-char-backward
        [remap evil-snipe-t]               #'evil-quickscope-find-char-to
        [remap evil-snipe-T]               #'evil-quickscope-find-char-to-backward)
  (after! (:or evil evil-snipe)
    (akn/prioritize-minor-mode-keymap 'evil-quickscope-mode))

  ;; HACK: Since evil-quickscope and evil-snipe both try to override the motion
  ;; state bindings for "t", "f", "T", and "F", `evil-quickscope' ends up
  ;; effectively overriding `evil-snipe-override-mode'. Here, we make
  ;; `evil-quickscope' aware of `evil-snipe-override-mode'.
  (defadvice! akn/evil-quickscope-snipe-a (args)
    :filter-args #'evil-quickscope-call-find
    (if (bound-and-true-p evil-snipe-override-local-mode)
        (pcase (car args)
          ('evil-find-char             (list #'evil-snipe-f))
          ('evil-find-char-backward    (list #'evil-snipe-F))
          ('evil-find-char-to          (list #'evil-snipe-t))
          ('evil-find-char-to-backward (list #'evil-snipe-T))
          (_ args))
      args))

  (setq! ;; evil-cross-lines t
         evil-snipe-scope 'whole-visible
         evil-quickscope-cross-lines t)

  (global-evil-quickscope-mode 1))

;;; evil respect for emacs `line-move-visual'
;; This is an alternative to `evil-respect-visual-line-mode'.
;; NOTE: Confusingly, the Emacs `visual-line-mode' (which is what we're talking about here)
;;       is unrelated to the evil/vim visual line state (V).

(when (modulep! :editor evil)
  ;; To make arrow keys consistent with insert state
  (map!
   :m "<down>" (akn/cmds! line-move-visual #'evil-next-visual-line     #'evil-next-line)
   :m "<up>"   (akn/cmds! line-move-visual #'evil-previous-visual-line #'evil-previous-line))

  (define-minor-mode akn/evil-respect-line-move-visual-mode
    ""
    :group 'akn
    :keymap (make-sparse-keymap))
  (map!
   :m "j"  (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-next-visual-line               #'evil-next-line)
   :m "gj" (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-next-line                      #'evil-next-visual-line)

   :m "k"  (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-previous-visual-line           #'evil-previous-line)
   :m "gk" (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-previous-line                  #'evil-previous-visual-line)

   :m "^"  (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-first-non-blank-of-visual-line #'evil-first-non-blank)
   :m "g^" (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-first-non-blank                #'evil-first-non-blank-of-visual-line)

   :m "$"  (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-end-of-visual-line             #'evil-end-of-line)
   :m "g$" (akn/cmds! (akn/should-use-evil-visual-line-p) #'evil-end-of-line                    #'evil-end-of-visual-line))
  ;; (evil-make-overriding-map akn/evil-respect-line-move-visual-mode-map)
  (defun akn/should-use-evil-visual-line-p ()
    (and line-move-visual
         akn/evil-respect-line-move-visual-mode
         ;; from https://github.com/YourFin/evil-better-visual-line
         (or (not evil-visual-state-minor-mode)
             (eq (evil-visual-type) 'inclusive))))

  (add-hook 'text-mode-hook #'akn/evil-respect-line-move-visual-mode)

  (after! so-long (add-to-list 'so-long-minor-modes 'akn/evil-respect-line-move-visual-mode)))

;;; evil shift-select-mode

(when (modulep! :editor evil)
  ;; ;; set `shift-select-mode' to `permanent' in normal/motion/visual states
  (defvar-local akn/evil-shift-select--restore nil)
  (add-hook! '(evil-motion-state-entry-hook evil-normal-state-entry-hook evil-visual-state-entry-hook)
    (defun akn/shift-select-permanent-h ()
      ;; (message "called")
      (when (eq (car-safe transient-mark-mode) 'only)
        (setq-local transient-mark-mode (cdr transient-mark-mode)))
      (push (buffer-local-set-state shift-select-mode 'permanent)
            akn/evil-shift-select--restore)))
  (add-hook! '(evil-motion-state-exit-hook evil-normal-state-exit-hook evil-visual-state-exit-hook)
    (defun akn/shift-select-restore-h ()
      ;; (message "undoing")
      (buffer-local-restore-state (pop akn/evil-shift-select--restore))))

  (defadvice! akn/evil-handle-shift-selection-a (&rest _)
    :before #'evil-previous-visual-line
    :before #'evil-next-visual-line
    :before #'evil-previous-line
    :before #'evil-next-line
    (when (called-interactively-p 'any)
      (handle-shift-selection))))

;;; evil-escape
(after! evil-escape
  ;; allow both jk and kj to be equivalent to escape
  (setq! evil-escape-key-sequence "jk"
         evil-escape-unordered-key-sequence t
         ;; original value = 0.1, doom increased it
         evil-escape-delay 0.07))

(map! :m "<escape>" (cmds! (not (memq evil-state '(operator replace)))
                           #'doom/escape))

;;; evil arrow keys crossing lines
;; To make arrow keys consistent with insert state
(evil-define-motion akn/evil-forward-char-cross-lines (count &optional crosslines noerror)
  (interactive "<c>" (prog1
                         (list t (evil-kbd-macro-suppress-motion-error))
                       (handle-shift-selection)))
  (evil-forward-char count crosslines noerror))
(evil-define-motion akn/evil-backward-char-cross-lines (count &optional crosslines noerror)
  (interactive "<c>" (prog1
                         (list t (evil-kbd-macro-suppress-motion-error))
                       (handle-shift-selection)))
  (evil-backward-char count crosslines noerror))
(map!
 :m "<left>"  (akn/cmds! (command-remapping #'evil-backward-char) it #'akn/evil-backward-char-cross-lines)
 :m "<right>" (akn/cmds! (command-remapping #'evil-forward-char)  it #'akn/evil-forward-char-cross-lines))

;;; make jk nav-flash in normal/motion/visual state

(when (modulep! :ui nav-flash)
  (defconst akn/jk-nav-flash--allowed-states '(motion normal visual))
  (defvar akn/jk-nav-flash--prev-key nil)
  (defvar akn/jk-nav-flash--prev-time '(0 . 0))
  (add-hook! 'post-command-hook #'akn/jk-nav-flash-h))

(defun akn/jk-nav-flash-h ()
  (when (memq evil-state akn/jk-nav-flash--allowed-states)
    (let* ((key (this-command-keys))
           (is-j (equal key "j"))
           (is-k (equal key "k")))
      (if (and (not is-j) (not is-k))
          (setq akn/jk-nav-flash--prev-key nil)
        (let* ((was-j (eq akn/jk-nav-flash--prev-key ?j))
               (was-k (eq akn/jk-nav-flash--prev-key ?k))
               (the-time (let ((current-time-list nil)) (current-time)))
               (time-diff (- (car the-time) (car akn/jk-nav-flash--prev-time))))
          (cond
           ((and (or (and was-j is-k) (and was-k is-j))
                 ;; is time difference less than 1/16 seconds?
                 (<= time-diff (akn/-> (cdr the-time) (ash -4))))
            (+nav-flash-blink-cursor)
            (setq akn/jk-nav-flash--prev-key nil))
           (t
            (setq akn/jk-nav-flash--prev-key (if is-j ?j ?k)
                  akn/jk-nav-flash--prev-time the-time))))))))

;;; evil-normalize-keymaps

;; https://github.com/emacs-evil/evil/issues/301
;; TODO: are there other places I should be doing `evil-normalize-keymaps'?
(add-hook! '(vlf-mode-hook
             evil-surround-mode-hook
             akn/terminal-quit-mode-hook
             evil-quickscope-mode-hook
             evil-quickscope-always-mode-hook
             outli-mode-hook
             akn/active-region-arrow-boundary-mode-hook akn/capf-autosuggest-mode-hook akn/secret-paste-mode-hook akn/vterm-escape-mode-hook akn/vterm-strict-mouse-mode-hook +multiple-cursors/mc-pause-cursors-mode-hook elisp-indent-docstrings-mode-hook electric-operator-mode-hook akn/auto-save-visited-local-mode-hook akn/immediate-auto-save-visited-local-mode-hook akn/local-smooth-scroll-disabled-mode-hook akn-keypad-mode-hook +transcribe-mode-hook git-auto-commit-mode-hook akn/github-explorer-file-mode-hook akn/modeline-word-count-mode-hook akn/gcmh-mode-hook akn/indent-guides-mode-hook akn/lsp-minimal-mode-hook akn/new-file-mode-hook akn/evil-WORD-symbol-mode-hook akn/evil-respect-visual-line-mode-hook +mistty-strict-mouse-mode-hook
             after-change-major-mode-hook
             ;; https://github.com/emacs-evil/evil/issues/301#issuecomment-591570732
             window-configuration-change-hook)
           :append
           #'evil-normalize-keymaps)

(akn/after-idle! (0.6 :each-idle t :timer-name akn/evil-normalize-keymaps-timer)
  ;; IMPORTANT: don't allow operator state here. Otherwise, evil operator
  ;; shortcuts (like "dd" and "gqq") will stop working.
  (when (and (memq evil-state '(normal motion insert))
             ;; not in a keymap set by `set-transient-map'
             (not (keymapp overriding-terminal-local-map))
             (not (and (fboundp 'which-key--popup-showing-p)
                       (which-key--popup-showing-p))))
    (evil-normalize-keymaps)))

;; evil-mc-mode evil-esc-mode evil-org-mode evil-tex-mode evil-lion-mode
;; evil-local-mode evil-snipe-mode evil-escape-mode evil-traces-mode
;; evil-goggles-mode evil-markdown-mode evil-surround-mode evil-list-view-mode
;; global-evil-mc-mode evil-org-agenda-mode evil-visualstar-mode
;; evil-snipe-local-mode evil-vimish-fold-mode evil-cleverparens-mode
;; evil-ace-jump-char-mode evil-ace-jump-line-mode evil-ace-jump-word-mode
;; evil-command-window-mode evil-snipe-override-mode akn/evil-WORD-symbol-mode
;; global-evil-surround-mode evil-ace-jump-char-to-mode
;; evil-operator-shortcut-mode global-evil-visualstar-mode
;; global-evil-vimish-fold-mode evil-save-transient-mark-mode
;; evil-with-transient-mark-mode evil-snipe-override-local-mode
;; akn/global-evil-WORD-symbol-mode evil-restore-transient-mark-mode

;;; pasting

;; see vim's :pu[t]
;; evil-ex-put
;; good for pasting rectangles (visual block) as lines without interspersing them with the existing text

(evil-define-command akn/evil-put-above (count &optional register yank-handler)
  (interactive "*P<x>")
  (setq count (or (prefix-numeric-value count) 1))
  (akn/evil-put-below (- count) register yank-handler))
(evil-define-command akn/evil-put-below (count &optional register _yank-handler)
  (interactive "*P<x>")
  (setq count (or (prefix-numeric-value count) 1))
  (let ((reversed (< count 0)))
    (dotimes (_ (abs count))
      (save-excursion
        (let ((pt (if (eobp) (point) (1+ (point)))))
          (evil-ex-put pt pt register reversed))))))

(map!
 :n "[ p" #'akn/evil-put-above
 :n "] p" #'akn/evil-put-below)

;;; repeat

(dolist (command akn/all-scroll-commands)
  (evil-add-command-properties command :repeat 'ignore))

;;; text objects

(put 'akn/defun-comments 'beginning-op #'beginning-of-defun-comments)
(put 'akn/defun-comments 'end-op       #'end-of-defun)
(put 'akn/defun-comments 'forward-op   #'end-of-defun)

(map! :textobj "f" #'+evil:defun-txtobj #'akn/+evil:defun-outer-txtobj)

(evil-define-text-object akn/+evil:defun-outer-txtobj (count &optional _beg _end type)
  "Text object to select the top-level Lisp form or function definition at
point, including any comments above."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'akn/defun-comments)
    (evil-range beg end type)))

;;; file-local variables

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
