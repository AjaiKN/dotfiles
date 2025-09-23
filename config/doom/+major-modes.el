;;; +major-modes.el -*- lexical-binding: t; -*-

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

(require 'akn)

;;; disable lsp-by-default
(defun akn/auto-lsp-p (&rest _)
  "`eglot' or `lsp-mode' are only enabled automatically if this returns non-nil."
  (not (or (bound-and-true-p so-long-minor-mode)
           (derived-mode-p 'nxml-mode
                           'json-mode
                           'sh-mode))))
(advice-add #'lsp! :before-while #'akn/auto-lsp-p)

;;; astro js
;; https://edmundmiller.dev/posts/emacs-astro/
(use-package! astro-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))
  (add-hook! 'astro-ts-mode-hook :append #'lsp!))

(set-formatter! 'prettier-astro
  '("npx" "prettier" "--parser=astro")
  ;; (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
  :modes '(astro-ts-mode))

(after! lsp-tailwindcss
  (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))

;;; conf-mode
;; https://docs.netlify.com/manage/routing/redirects/overview/
(add-to-list 'auto-mode-alist '("/_redirects\\'" . conf-space-mode))

;;; csv

(setq-hook! 'csv-mode-hook
  buffer-read-only t)

(add-hook 'csv-mode-hook #'csv-align-mode)

;;; dired/dirvish
(map! :after dired
      :map (dired-mode-map wdired-mode-map)
      "M-p"        #'dired-prev-dirline
      "M-n"        #'dired-next-dirline
      "s->"        #'dired-omit-mode
      "M-l"        #'dirvish-ls-switches-menu
      [remap +multiple-cursors/click-add-cursor] #'ignore
      :gnm "<mouse-2>" #'dired-mouse-find-file
      :gnm "s-<mouse-2>" #'dired-mouse-find-file-other-window
      :nviemg "s-<return>" #'dired-find-file-other-window
      :mn "<tab>" (if (modulep! :emacs dired +dirvish) #'dirvish-subtree-toggle #'akn/dired-hide-subdir)
      :mn "TAB"   (if (modulep! :emacs dired +dirvish) #'dirvish-subtree-toggle #'akn/dired-hide-subdir)
      :mn "<backtab>" (if (modulep! :emacs dired +dirvish) #'dirvish-subtree-clear #'dired-hide-all)
      :mn "S-<tab>"   (if (modulep! :emacs dired +dirvish) #'dirvish-subtree-clear #'dired-hide-all)
      :mn "S-TAB"     (if (modulep! :emacs dired +dirvish) #'dirvish-subtree-clear #'dired-hide-all))

(use-package! dired
  :defer-incrementally (dired-loaddefs dnd)
  :config
  (setq! dired-movement-style 'cycle-files
         dirvish-use-header-line nil ; dirvish's header line makes cycling not work right
         dired-listing-switches "-ahl -v --group-directories-first")
  (setq-hook! 'dired-mode-hook
    line-move-visual nil))

(defun akn/dired-goto-beginning ()
  (interactive nil dired-mode)
  (goto-char (point-min))
  (redisplay)
  (let ((dired-movement-style 'cycle-files))
    (goto-char (point-max))
    (dired-next-line 1)))
(defun akn/dired-goto-end ()
  (interactive)
  (let ((dired-movement-style 'cycle-files))
    (goto-char (point-min))
    (dired-previous-line 1)))
(map! :after dired
      :map dired-mode-map
      [remap beginning-of-buffer]  (cmds! (not current-prefix-arg) #'akn/dired-goto-beginning)
      [remap end-of-buffer]        (cmds! (not current-prefix-arg) #'akn/dired-goto-end)
      [remap evil-goto-first-line] (cmds! (not current-prefix-arg) #'akn/dired-goto-beginning)
      [remap evil-goto-line]       (cmds! (not current-prefix-arg) #'akn/dired-goto-end)
      [remap evil-next-line]            #'dired-next-line
      [remap evil-previous-line]        #'dired-previous-line
      [remap evil-next-visual-line]     #'dired-next-line
      [remap evil-previous-visual-line] #'dired-previous-line)

(defun akn/dired-hide-subdir ()
  (interactive nil dired-mode)
  (save-excursion
    (call-interactively (if (save-excursion
                              (and (dired-file-name-at-point)
                                   (directory-name-p (dired-file-name-at-point))))
                            #'dired-maybe-insert-subdir
                          #'dired-hide-subdir))))
(defun akn/insert-or-hide-subdir ()
  (interactive nil dired-mode))

(use-package! dirvish
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "home")
     ("p" "~/prog/"                     "prog")
     ("o" "~/Documents/obsidian-vault/" "obsidian vault")
     ("," "~/.config/doom/"             "private config")
     ("d" "~/.config/emacs/"            "doom source")
     ("/" "/"                           "root")
     ("t" "~/.Trash/"                   "trash")))
  :config
  (setq! dirvish-subtree-listing-switches "-Ahl -v --group-directories-first"
         dirvish-subtree-prefix "  │ ")
  (pushnew! dirvish-attributes 'collapse))

(after! (:or dired dirvish)
  (advice-remove #'dired-find-file #'dirvish-find-entry-a))
(akn/undefine-advice dired-find-file (:around (fn &rest args) akn/dirvish-find-entry-a)
  (advice-remove #'dired-find-file #'dirvish-find-entry-a)
  (condition-case-unless-debug err
      (apply #'dirvish-find-entry-a args)
    (error
     (doom-log "Error in `dirvish-find-entry-a', called from `akn/dirvish-find-entry-a': %S" err)
     (apply fn args))))

(defadvice! akn/dired--align-a (&rest _)
  "Move point to the filename."
  :before #'dirvish-subtree-toggle
  :before #'dired-find-file
  (when (derived-mode-p 'dired-mode)
    (let (dired-movement-style)
      (dired-next-line 0))))
(add-hook! 'dired-mode-hook
  (defun akn/dired--align-h (&rest _)
    "Move point to the filename."
    (akn/after-idle! (0 :timer-name akn/dired--align-timer)
      (when (derived-mode-p 'dired-mode)
        (akn/dired--align-a)))))

(define-advice dired-rename-file (:after (file newname &rest _) akn/update-refs-a)
  (ignore-errors
    (doom-require 'doom-lib 'files)
    (doom-files--update-refs file newname))
  (ignore-errors (when (projectile-project-root) (projectile-invalidate-cache nil))))
(define-advice dired-copy-file (:after (_from to &rest _) akn/update-refs-a)
  (ignore-errors
    (doom-require 'doom-lib 'files)
    (doom-files--update-refs to))
  (ignore-errors (when (projectile-project-root) (projectile-invalidate-cache nil))))
(define-advice dired-delete-file (:after (file &rest _) akn/update-refs-a)
  (ignore-errors
    (doom-require 'doom-lib 'files)
    (doom-files--update-refs file))
  (ignore-errors (when (projectile-project-root) (projectile-invalidate-cache nil))))
(define-advice dired-create-empty-file (:after (file &rest _) akn/update-refs-a)
  (ignore-errors
    (doom-require 'doom-lib 'files)
    (doom-files--update-refs file))
  (ignore-errors (when (projectile-project-root) (projectile-invalidate-cache nil))))

;; https://xenodium.com/interactive-ordering-of-dired-items
(after! dired
  (defun ar/dired-drag-item-up ()
    "Drag dired item down in buffer."
    (interactive)
    (unless (dired-get-filename nil t)
      (error "Not a dired draggable item"))
    (when (= (line-number-at-pos) 2)
      (error "Already at top"))
    (let* ((inhibit-read-only t)
           (col (current-column))
           (item-start (line-beginning-position))
           (item-end (1+ (line-end-position)))
           (item (buffer-substring item-start item-end)))
      (delete-region item-start item-end)
      (forward-line -1)
      (beginning-of-line)
      (insert item)
      (forward-line -1)
      (move-to-column col)))

  (defun ar/dired-drag-item-down ()
    "Drag dired item down in buffer."
    (interactive)
    (unless (dired-get-filename nil t)
      (error "Not a dired draggable item"))
    (when (save-excursion
            (forward-line 1)
            (eobp))
      (error "Already at bottom"))
    (let* ((inhibit-read-only t)
           (col (current-column))
           (item-start (line-beginning-position))
           (item-end (1+ (line-end-position)))
           (item (buffer-substring item-start item-end)))
      (delete-region item-start item-end)
      (forward-line 1)
      (beginning-of-line)
      (insert item)
      (forward-line -1)
      (move-to-column col)))

  (map! :map dired-mode-map
        [remap drag-stuff-up] #'ar/dired-drag-item-up
        [remap drag-stuff-down] #'ar/dired-drag-item-down
        [remap +fold/drag-stuff-up] #'ar/dired-drag-item-up
        [remap +fold/drag-stuff-down] #'ar/dired-drag-item-down))

;;; treemacs
;; Otherwise, when treemacs-follow-mode gets loaded (incrementally),
;; it immediately gets turned on and runs treemacs--follow-after-buffer-list-update constantly.
;; If we do want to enable treemacs-follow-mode, we shouldn't do that until treemacs is actually loaded.
(after! treemacs-follow-mode
  (unless (featurep 'treemacs)
    (treemacs-follow-mode -1)))
(use-package! treemacs
  :init
  ;; `deferred' was causing weird bugs, for example when running `treemacs-toggle-show-dotfiles'.
  ;; If this is slow, try `simple'.
  (setq +treemacs-git-mode 'extended)
  :config
  ;; might be cleaner to use advice for these
  (defun akn/treemacs-leftclick-action (event)
    (interactive "e")
    (setf (elt event 0) 'down-mouse-1)
    (treemacs-leftclick-action event))
  (defun akn/treemacs-single-click-expand-action (event)
    (interactive "e")
    (setf (elt event 0) 'mouse-1)
    (treemacs-single-click-expand-action event))
  (defun akn/treemacs-doubleclick-action (event)
    (interactive "e")
    (setf (elt event 0) 'double-mouse-1)
    (treemacs-doubleclick-action event))

  (map! :map treemacs-mode-map
        ;; "s->" #'treemacs-toggle-show-dotfiles
        "s->" (akn/cmds! treemacs-git-mode #'treemacs-hide-gitignored-files-mode #'treemacs-toggle-show-dotfiles)
        "s-<return>" #'treemacs-visit-node-horizontal-split
        :map evil-treemacs-state-map "<right>" #'treemacs-TAB-action
        :map evil-treemacs-state-map "<left>" #'treemacs-COLLAPSE-action

        ;; https://github.com/Alexander-Miller/treemacs?tab=readme-ov-file#mouse-interface
        "<mouse-1>" #'treemacs-single-click-expand-action
        "S-<mouse-1>" #'ignore-preserving-kill-region
        "S-<down-mouse-1>" #'akn/treemacs-leftclick-action
        "S-<double-down-mouse-1>" #'akn/treemacs-doubleclick-action
        "s-<mouse-1>" (defun akn/treemacs-command-click (event)
                        (interactive "e")
                        (let ((treemacs-doubleclick-actions-config
                               `((file-node-open   . treemacs-visit-node-horizontal-split)
                                 (file-node-closed . treemacs-visit-node-horizontal-split)
                                 ,@treemacs-doubleclick-actions-config)))
                          (akn/treemacs-single-click-expand-action event))))

  (add-hook! 'treemacs-mode-hook
    (defun akn/hide-mode-line-mode ()
      (hide-mode-line-mode)
      (akn/after-timer! (0)
        (hide-mode-line-mode)
        (redraw-display))))
  (add-hook 'treemacs-mode-hook #'treemacs-follow-mode)
  (defalias 'akn/treemacs-preview-mode #'treemacs-peek-mode))

;;; ibuffer
(after! ibuffer
  (add-hook! 'ibuffer-mode-hook #'ibuffer-auto-mode))

;;; bookmark
(map! :after bookmark
      :map bookmark-bmenu-mode-map
      :nviemg "J" #'bookmark-jump)

;;; help

(map! [remap describe-key] nil)

(after! help-mode
  (defadvice! akn/help-previous-override-a (&rest _args)
    :before-until #'previous-buffer
    (when (and (derived-mode-p 'help-mode) help-xref-stack)
      (help-go-back)
      t))
  (defadvice! akn/help-next-override-a (&rest _args)
    :before-until #'next-buffer
    (when (and (derived-mode-p 'help-mode) help-xref-forward-stack)
      (help-go-forward)
      t)))

(define-advice describe-symbol (:around (fn symbol &rest args) akn/helpful-a)
  (if (and (null args) (called-interactively-p 'interactive))
      (akn/describe symbol)
    (apply fn symbol args)))

(defun akn/describe (thing)
  (when-let* (((stringp thing))
              (sym (intern-soft thing))
              ((cl-some (lambda (x) (funcall (nth 1 x) sym))
                        describe-symbol-backends)))
    (setq thing sym))
  (cond
   ((and (symbolp thing)
         (> (seq-count (lambda (x) (funcall (nth 1 x) thing))
                       describe-symbol-backends)
            1))
    (describe-symbol thing))
   ((or (keymapp thing)
        (and (symbolp thing) (boundp thing) (keymapp (symbol-value thing))))
    (message "describe: %s" thing)
    (setq thing (akn/keymap-symbol-maybe thing))
    (message "desccribing %s" thing)
    (or (and (symbolp thing)
             (fboundp 'helpful-variable)
             (ignore-errors (helpful-variable thing) t))
        (describe-keymap thing)))
   ((and (stringp thing) (ignore-errors (fontset-name-p thing)))
    (describe-fontset thing))
   ((and (symbolp thing) (ignore-errors (fontset-name-p (symbol-name thing))))
    (describe-fontset (symbol-name thing)))
   ((ignore-errors (fontp thing))
    (condition-case nil
        (describe-font thing)
      (error
       ;; copied from `describe-font'
       (let ((xref-item (list #'describe-font fontname))
             (font-info (font-info fontname))
             (help-buffer-under-preparation t))
         (if (null font-info)
             (if (fontp fontname 'font-object)
                 (message "No information about \"%s\"" (font-xlfd-name fontname))
               (message "No matching font found"))
           (help-setup-xref xref-item (called-interactively-p 'interactive))
           (with-output-to-temp-buffer (help-buffer)
             (describe-font-internal font-info)))))))
   ((and (symbolp thing)
         (seq-some (lambda (x) (funcall (nth 1 x) thing)) describe-symbol-backends))
    (or (and (fboundp 'helpful-symbol) (ignore-errors (helpful-symbol thing) t))
        (describe-symbol thing)))
   ((or (arrayp thing)
        (and (consp thing)
             (arrayp (car thing))
             (arrayp (cdr thing))))
    (let ((ret (describe-key thing)))
      (if (and (stringp ret) (stringp thing) (string-match-p (rx " is undefined" eos) ret))
          (describe-key (kbd thing))
        ret)))
   (t (user-error "akn/describe: unrecognized thing: %s" thing))))
(defalias 'akn/help #'akn/describe)

;;; prefix help command
(defun akn/keymap-symbol-maybe (keymap)
  "Return the symbol to which KEYMAP is bound, or the keymap itself
if no such symbol exists."
  (or (if (symbolp keymap)
          (ignore-errors (indirect-variable keymap))
        (require 'help-fns)
        (help-fns-find-keymap-name keymap))
      keymap))

;; https://karthinks.com/software/persistent-prefix-keymaps-in-emacs/
(defun akn/repeated-prefix-help-command ()
  (interactive)
  (when-let* ((keys (this-command-keys-vector))
              (prefix (seq-take keys (1- (length keys))))
              (orig-keymap (key-binding prefix 'accept-default))
              (keymap (copy-keymap orig-keymap))
              (exit-func (set-transient-map keymap t #'which-key-abort)))
    ;; TODO: doesn't seem to always get deactivated?
    (define-key keymap [remap keyboard-quit] (lambda () (interactive) (message "Exiting repeated prefix") (funcall exit-func)))
    (add-transient-hook! 'doom-escape-hook
      (message "Exiting repeated prefix")
      (funcall exit-func))
    (which-key--create-buffer-and-show nil keymap)))

(defun akn/minibuffer-bindings-in-keymap ()
  (interactive)
  (let* ((keys (this-command-keys))
         (prefix (seq-take keys (1- (length keys))))
         (keymap (key-binding prefix 'accept-default)))
    (if (fboundp 'embark-bindings-in-keymap)
        (minibuffer-with-setup-hook
            (lambda ()
              (let ((pt (- (minibuffer-prompt-end) 2)))
                (overlay-put (make-overlay pt pt) 'before-string
                             (format " under %s" (key-description prefix)))))
          (embark-bindings-in-keymap keymap))
      (describe-prefix-bindings))))

(defun akn/help-next-key ()
  (interactive)
  (let* ((keys (this-command-keys))
         (prefix (seq-take keys (1- (length keys))))
         (keymap (key-binding prefix 'accept-default))
         (help-key-used (seq-elt keys (1- (length keys))))
         (key (read-key "enter key: ")))
    (akn/describe
     (if (eq key help-key-used)              ;@@ means see docs for current keymap
         keymap
       (keymap-lookup keymap (key-description (string key)))))))

;; `describe-prefix-bindings': Emacs default
;; `embark-prefix-help-command': Doom default
;; `which-key-C-h-dispatch': which-key default
(defvar-keymap akn/prefix-help-map
  "C-h" #'akn/minibuffer-bindings-in-keymap
  "@" #'akn/help-next-key
  "H-j" #'which-key-show-next-page-cycle
  "H-k" #'which-key-show-previous-page-cycle
  "<f1>" #'which-key-C-h-dispatch
  "DEL" #'which-key-undo-key
  "<help>" #'akn/minibuffer-bindings-in-keymap
  "?" #'akn/minibuffer-bindings-in-keymap
  "s-y" #'akn/repeated-prefix-help-command)

(defun akn/update-prefix-help ()
  (setq help-event-list nil
        prefix-help-command #'akn/prefix-help-command)
  (map-keymap
   (lambda (event _definition)
     (when (or (numberp event) (symbolp event))
       (push event help-event-list)))
   akn/prefix-help-map))
(akn/update-prefix-help)

(lookup-key akn/prefix-help-map (vector ?\H-j))

(setq help-char ?\C-h)

(after! which-key
  (add-to-list 'which-key--paging-functions #'akn/prefix-help-command))
(defun akn/prefix-help-command ()
  (interactive)
  ;; (message "hi %s" (which-key--popup-showing-p))
  (let* ((keys (this-command-keys))
         (help-key-used (seq-elt keys (1- (length keys)))))
    (call-interactively (lookup-key akn/prefix-help-map (vector help-key-used)))))

;;; helpful (docs)
(after! (:or find-func helpful)
  ;; Set C source directory
  ;; can download from https://gnu.askapache.com/emacs/
  (setq! find-function-C-source-directory (or find-function-C-source-directory
                                              (akn/existing-file-truename (concat "~/Downloads/emacs-" emacs-version "/src")))))
;; https://github.com/Wilfred/helpful/issues/250#issuecomment-954262620
(after! helpful
  ;; Only needed when using Doom Emacs with (modulep! :ui popup).
  (set-popup-rule! (rx bol "*helpful ") :size 0.35 :ttl nil :select t)
  ;; In helpful-mode, override previous-buffer and next-buffer to
  ;; run (*helpful-previous) or (*helpful-next) instead.

  (defadvice! akn/helpful-previous-override-a (&rest _args)
    :before-until #'previous-buffer
    (when (akn/should-override-helpful-prev-buffer)
      (+emacs-lisp/helpful-previous)))
  (defadvice! akn/helpful-next-override-a (&rest _args)
    :before-until #'next-buffer
    (when (akn/should-override-helpful-prev-buffer)
      (+emacs-lisp/helpful-next)))
  (defun akn/should-override-helpful-prev-buffer ()
    (and (derived-mode-p 'helpful-mode)
         ;; if previous-buffer and next-buffer wouldn't work anyway
         ;; - for example, when the helpful window is maximized with SPC w m m, I don't want this
         (window-dedicated-p (frame-selected-window)))))

;;; ielm
(map!
 "C-:" #'akn/ielm
 "s-:" #'akn/ielm)
(defun akn/elisp-repl ()
  (interactive)
  (let ((b (current-buffer)))
    (ielm)
    (when (modulep! :editor evil) (evil-insert-state))
    (ielm-change-working-buffer b)))
(when (modulep! :editor evil)
  (add-to-list 'evil-insert-state-modes 'inferior-emacs-lisp-mode))
(defalias 'akn/ielm #'akn/elisp-repl)

;;; info (docs)
;; see `evil-collection-info-setup' for default keybindings

(after! (:and info evil-collection-info)
  ;; double-wheel-left and double-wheel-right seem to not work very well in emacs-mac.
  ;; previous-buffer and next-buffer still get called.
  (evil-collection-define-key 'normal 'Info-mode-map
    [double-wheel-left]  nil
    [double-wheel-right] nil))

;; ;; In Info-mode, override previous-buffer and next-buffer to
;; ;; run (Info-history-back) or (Info-history-forward) instead.
;; (undefadvice! akn/Info-previous-override-a (&rest _args)
;;   :before-until #'previous-buffer
;;   (when (derived-mode-p 'Info-mode)
;;     ;; TODO: catch the user-error that happens if there's no more Info history
;;     (condition-case nil
;;         (progn (Info-history-back)
;;                t)
;;       (user-error
;;        nil))))

;; (undefadvice! akn/Info-next-override-a (&rest _args)
;;   :before-until #'next-buffer
;;   (when (derived-mode-p 'Info-mode)
;;     (condition-case nil
;;         (progn (Info-history-forward)
;;                t)
;;       (user-error
;;        nil)))))

(defun akn/Info-follow-nearest-node (&optional fork)
  "Like `Info-follow-nearest-node', but always works if there's a link on
this line, even if it's not on point.

This is to mimic the behavior of RET in Stand-alone GNU Info."
  (interactive "P" Info-mode)
  (cl-block succeeded
    (let ((orig-point (point)))
      (condition-case ()
          (Info-follow-nearest-node fork)
        (:success (cl-return-from succeeded))
        (user-error
         (beginning-of-line)
         (while (not (eolp))
           (condition-case ()
               (Info-follow-nearest-node fork)
             (:success (cl-return-from succeeded))
             (user-error
              (forward-char))))))
      (goto-char orig-point)
      (user-error "No reference or menu item on line"))))

(map! :after (:or info evil-collection-info)
      :map Info-mode-map
      [remap Info-follow-nearest-node] #'akn/Info-follow-nearest-node
      [remap quit-window] (akn/cmds! (frame-parameter nil 'akn/Info-standalone)
                                     #'save-buffers-kill-terminal)
      "M-[" (akn/cmds! (display-graphic-p) #'Info-history-back)
      "M-]" (akn/cmds! (display-graphic-p) #'Info-history-forward)
      "s-[" #'Info-history-back
      "s-]" #'Info-history-forward
      :nviemorg "<" #'Info-top-node
      :nviemorg ">" #'Info-final-node
      :mn "^" #'Info-up
      :mn "c" #'Info-copy-current-node-name
      :mn "g w" #'Info-goto-node-web
      ;; :mn "h" #'Info-help
      :mn "H" #'Info-help
      :mn "?" #'Info-summary
      :mn "f" #'Info-scroll-up
      :mn "b" #'Info-scroll-down
      ;; :mn "1" #'Info-nth-menu-item
      ;; :mn "2" #'Info-nth-menu-item
      ;; :mn "3" #'Info-nth-menu-item
      ;; :mn "4" #'Info-nth-menu-item
      ;; :mn "5" #'Info-nth-menu-item
      ;; :mn "6" #'Info-nth-menu-item
      ;; :mn "7" #'Info-nth-menu-item
      ;; :mn "8" #'Info-nth-menu-item
      ;; :mn "9" #'Info-nth-menu-item
      :mn "[ [" #'Info-backward-node
      :mn "] ]" #'Info-forward-node
      :mn "p" #'Info-prev
      :mn "n" (akn/cmds! (not (or (bound-and-true-p isearch-opened-overlays)
                                  (evil-ex-hl-active-p 'evil-ex-search)
                                  (bound-and-true-p anzu--state)))
                         #'Info-next))

;; TUI new:      emacs-term-new --eval '(akn/info-standalone "info")'
;; GUI new:      emacs-open-new --eval '(akn/info-standalone "info")'
;; TUI existing: emacs-term --eval '(akn/info-standalone "info")'
;; GUI existing: emacs-frame-new --eval '(akn/info-standalone "info")'
(defun akn/info-standalone (&optional topic)
  (setq topic (or topic
                  (and command-line-args-left
                       (not (string-match "^-" (car command-line-args-left)))
                       (car command-line-args-left))))
  (let ((display-buffer-overriding-action (cons #'display-buffer-same-window nil)))
    (info topic))
  (let ((buf (current-buffer)))
    (setf (frame-parameter nil 'akn/Info-standalone) t)
    (after! tab-line
      (akn/after-timer! (0)
        (with-current-buffer buf
          (when (buffer-live-p buf)
            (tab-line-mode -1)))))
    (after! tab-bar
      (akn/after-timer! (0)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (> (frame-parameter nil 'tab-bar-lines) 0)
              (toggle-frame-tab-bar))))))))

(after! info
  (pushnew! Info-directory-list)
  (cl-callf append
      Info-additional-directory-list
      (doom-glob (or (getenv-internal "HOMEBREW_CELLAR") "/opt/homebrew/Cellar")
                 "texinfo/*/share/info")
      (doom-glob "/opt/R/*/gfortran/share/info")))

;;; json

(when (modulep! :lang json +lsp)
  (remove-hook 'json-mode-local-vars-hook #'lsp!)
  (define-minor-mode akn/json-always-lsp-mode
    "If enabled, always enable LSP for json files."
    :global t
    :group 'akn)
  (add-hook! 'json-mode-local-vars-hook :append
    (defun akn/json-maybe-lsp-h ()
      (when (or akn/json-always-lsp-mode
                (and (not (bound-and-true-p so-long-minor-mode))
                     (save-excursion
                       (let ((case-fold-search nil))
                         (save-restriction
                           (goto-char (point-min))
                           (search-forward "\"$schema\""
                                           (min (point-max)
                                                (+ (point-min) magic-mode-regexp-match-limit))
                                           t))))))
        (lsp!)))))

;;;; jsonl (json lines)

(when (modulep! :lang json)
  (define-derived-mode +json-lines-mode json-mode "JSONL"
    "A major mode for editing JSON Lines (JSONL) files."
    :group 'akn)
  (unless (modulep! :lang jsonian)
    (add-to-list 'auto-mode-alist '("\\.jsonl\\'" . +json-lines-mode))))

;;; akn/keymapper-conf-mode

(define-derived-mode akn/keymapper-conf-mode conf-unix-mode "Keymapper"
  "Major mode for editing keymapper configuration file.

See URL `https://github.com/houmain/keymapper'."
  (conf-mode-initialize "#"))

(add-to-list 'auto-mode-alist
             (cons (rx (or (: "/keymapper.conf" eos)
                           (: "/keymapper/" (* anychar) ".conf" eos)))
                   #'akn/keymapper-conf-mode))

(when (modulep! :checkers syntax -flymake)
  (add-hook 'akn/keymapper-conf-mode-hook #'flycheck-mode)
  (after! flycheck
    (flycheck-def-executable-var akn/keymapper "keymapper")
    (flycheck-define-command-checker 'akn/keymapper
      "Checker for `akn/keymapper-conf-mode'."
      :command '("keymapper" "--check" "--config" source)
      :error-patterns '((error line-start "ERROR: " (message) (? "in file '" (file-name) "'") (? " in line " line) line-end))
      :error-filter
      (lambda (errs)
        (flycheck-fill-empty-line-numbers errs)
        (flycheck-sanitize-errors errs))
      :modes '(akn/keymapper-conf-mode))
    (add-to-list 'flycheck-checkers 'akn/keymapper)))

;;; launchctl
(use-package! nxml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode)))

(use-package! launchctl
  :config
  ;; https://github.com/syl20bnr/spacemacs/blob/a58a7d79b3713bcf693bb61d9ba83d650a6aba86/layers/%2Bos/osx/packages.el#L69C1-L88C32
  (when (modulep! :editor evil)
    (add-to-list 'evil-motion-state-modes 'launchctl-mode))
  ;; (akn/defvar-setq akn/launchctl-search-path1 '("~/Library/LaunchAgents/" "/Library/LaunchAgents/" "/Library/LaunchDaemons/" "/System/Library/LaunchAgents/" "/System/Library/LaunchDaemons/"))
  ;; (akn/defvar-setq akn/launchctl-search-path2 '("~/Library/LaunchAgents/"))
  ;; (setq! launchctl-search-path akn/launchctl-search-path2)
  ;; (defun akn/toggle-all-search-paths ()
  ;;   (interactive)
  ;;   (setq-local launchctl-search-path (if (equal launchctl-search-path akn/launchctl-search-path2)
  ;;                                         akn/launchctl-search-path1
  ;;                                       akn/launchctl-search-path2)))
  (map!
   (:map launchctl-mode-map
    ;; :nm "t" #'akn/toggle-all-search-paths
    :desc "Refresh"                     :nm "g" #'launchctl-refresh
    :desc "Quit"                        :nm "q" #'kill-current-buffer
    :desc "Sort list"                   :nm "s" #'tabulated-list-sort
    :desc "New service config file"     :nm "n" #'launchctl-new
    :desc "Edit config file"            :nm "e" #'launchctl-edit
    :desc "View config file read-only"  :nm "v" #'launchctl-view
    :desc "Load service"                :nm "l" #'launchctl-load
    :desc "Unload service"              :nm "u" #'launchctl-unload
    :desc "Reload service"              :nm "r" #'launchctl-reload
    :desc "Disable service permanently" :nm "d" #'launchctl-disable
    :desc "Enable service permanently"  :nm "E" #'launchctl-enable
    :desc "Start service"               :nm "S" #'launchctl-start
    :desc "Stop service"                :nm "K" #'launchctl-stop
    :desc "Remove service"              :nm "D" #'launchctl-remove
    :desc "Service info"                :nm "i" #'launchctl-info
    :desc "Filter by regex"             :nm "f" #'launchctl-filter
    :desc "Clear filters"               :nm "F" (cmd! (setq launchctl-filter-regex ".") (launchctl-refresh))
    :desc "Set env variable"            :nm "=" #'launchctl-setenv
    :desc "Unset env variable"          :nm "#" #'launchctl-unsetenv
    :desc "Help"                        :nm "h" #'which-key-show-major-mode)))

;;; lisp

(after! sly
  (set-popup-rule! (rx bol "*sly-mrepl") :slot 3 :side 'right :size 0.5 :select t :quit nil)
  (set-popup-rule! (rx bol "*aprepl")    :slot 2 :side 'right :size 0.5 :select t :quit nil)
  (set-popup-rule! (rx bol "*sly-db")    :slot 1 :side 'right :size 0.5 :select t :quit nil :height 0.8))

(set-lookup-handlers! '(sly-db-mode sly-mrepl-mode)
  :definition #'sly-edit-definition
  :documentation #'sly-describe-symbol)

(after! sly-mrepl
  (set-keymap-parent sly-mrepl-mode-map comint-mode-map)
  (add-hook! 'sly-mrepl-mode-hook
    (defun akn/sly-keymap-parent-h ()
      (set-keymap-parent sly-mrepl-mode-map comint-mode-map))))

(after! lisp-mode
  (map! :map (lisp-mode-map sly-editing-mode-map)
        "C-c C-c" #'sly-eval-buffer
        "C-c C-S-c" #'sly-compile-defun
        "C-c C-o" #'sly-compile-defun
        "C-c h o" #'sly-describe-symbol
        "C-c h f" #'sly-describe-function
        "C-c q" #'sly-quickload)

  (map! [remap sly-quickload] #'akn/sly-quickload)
  (defun akn/sly-quickload (system)
    (interactive
     (let ((current-system (when-let* ((f (sly-asdf-find-system-file default-directory)))
                             (file-name-base f))))
       (list (akn/completing-read (append (if current-system (list current-system))
                                          (sly-eval
                                           '(slynk-quicklisp:available-system-names)))
                                  :prompt "QL system? "
                                  :default (if current-system (list current-system))))))
    (sly-quickload system)))

(after! sly
  (setq! sly-complete-symbol-function 'sly-flex-completions
         sly-command-switch-to-existing-lisp 'always)

  (add-hook 'sly-db-mode-hook (akn/mode-disabler #'evil-snipe-local-mode)))

(map! :map sly-db-mode-map
      [remap sly-db-continue] #'akn/sly-db-continue)
(defun akn/sly-db-continue ()
  "Invoke the \"continue\" or \"record-event\" restart."
  (interactive)
  (cl-assert sly-db-restarts () "akn/sly-db-continue called outside of sly-db buffer")
  (akn/sly-db-invoke-one-of '("continue" "record-event")))
(defun akn/sly-db-invoke-one-of (restarts)
  (let ((available-restarts (mapcar #'downcase (mapcar #'car sly-db-restarts))))
    (if-let* ((restart-num (seq-some (lambda (restart) (seq-position available-restarts restart))
                                     restarts)))
        (sly-rex ()
            (`(slynk:invoke-nth-restart-for-emacs ,sly-db-level ,restart-num))
          ((:ok _)
           (message "Failed!"))
          ((:abort _)))
      (sly-message "No continue restarts found (%s)" (string-join restarts ", "))
      (ding))))

(after! (:or emacs sly)
  (when (boundp 'sly-contribs)
    (add-to-list 'sly-contribs 'sly-mrepl)))
(after! (:and sly sly-mrepl)
  (akn/load! "./lisp/sly-mrepl-sly-db")
  (map! :map sly-db-mode-map
        :mng "E" #'sly-db-mrepl-prompt-in-frame))

;;;; aprepl

;; https://github.com/phantomics/april/tree/master/aprepl

(defun akn/autoload-aprepl ()
  (require 'sly)
  (when-let* ((aprepl-file
               (sly-eval '(cl:namestring (asdf:system-relative-pathname :april "aprepl/aprepl.el")))))
    (load aprepl-file)
    t))

(unless (fboundp 'aprepl)
  (defun aprepl (&optional buf-name)
    "Interactively evaluate April APL expressions.
Switches to the buffer named BUF-NAME if provided (`*aprepl*' by default),
or creates it if it does not exist."
    (interactive)
    (if (akn/autoload-aprepl)
        (aprepl buf-name)
      (error "aprepl.el not found"))))

;;;; "try" testing library
(defun akn/load-mgl-try ()
  (interactive)
  (require 'sly)
  (sly-eval-async '(cl:progn
                     (ql:quickload "try")
                     (try:install-try-elisp "~/quicklisp/"))
    (lambda (_res)
      (load "~/quicklisp/mgl-try.el"))))

(after! mgl-try
  (set-popup-rule! (rx "*try*") :size 0.2 :select nil)
  (akn/undefine-advice mgl-try-try (:around (fn &rest args) akn)
    (save-selected-window
      (apply fn args)))
  (define-advice mgl-try-display (:around (fn &rest args) akn/a)
    (let ((switch-to-buffer-obey-display-actions t))
      (apply fn args)
      (akn/mgl-try-result-mode)))
  (define-minor-mode akn/mgl-try-result-mode
    ""
    :group 'akn
    :keymap (make-sparse-keymap))
  (map! :map akn/mgl-try-result-mode-map
        :mn "q" #'quit-window)

  ;; pretend sly is slime
  (defvaralias 'slime-minibuffer-map 'sly-minibuffer-map)
  (defvar slime-completion-at-point-functions
    '(sly-complete-filename-maybe
      sly-complete-symbol))
  (defun slime--completion-at-point ()
    (cond (sly-complete-symbol-function
           sly-complete-symbol-function)
          (t
           (run-hook-with-args-until-success
            'slime-completion-at-point-functions))))
  (defun slime-setup-completion ()
    (add-hook 'completion-at-point-functions #'slime--completion-at-point nil t))
  (defun slime-minibuffer-setup-hook ()
    (cons (let ((package (sly-current-package))
                (connection (sly-connection)))
            (lambda ()
              (setq sly-buffer-package package)
              (setq sly-buffer-connection connection)
              (set-syntax-table lisp-mode-syntax-table)
              (slime-setup-completion)))
          minibuffer-setup-hook))
  (defalias 'slime-eval #'sly-eval)
  (defalias 'slime-eval-async #'sly-eval-async)
  (defalias 'slime-symbol-at-point #'sly-symbol-at-point))

;; (defadvice! akn/sly-emulate-slime-a (fn &rest args)
;;   :around #'akn/load-mgl-try
;;   :around #'mgl-try
;;   :around #'mgl-try-try
;;   :around #'mgl-try-mode
;;   :around #'mgl-try-display
;;   :around #'mgl-try-rerun-!
;;   :around #'mgl-try-next-regexp
;;   :around #'mgl-try-rerun-!-all
;;   :around #'mgl-try-next-unexpected
;;   :around #'mgl-try-previous-regexp
;;   :around #'mgl-try-insert-with-face
;;   :around #'mgl-try-previous-unexpected
;;   :around #'mgl-try-read-from-minibuffer
;;   :around #'mgl-try-next-not-expected-success
;;   :around #'mgl-try-previous-not-expected-success
;;   ;; (defalias 'slime-symbol-at-point #'sly-symbol-at-point)
;;   (akn/letf! ((slime-minibuffer-setup-hook sly-minibuffer-setup-hook)
;;               (slime-minibuffer-map sly-minibuffer-map)
;;               (#'slime-eval #'sly-eval)
;;               (#'slime-eval-async #'sly-eval-async)
;;               (#'slime-symbol-at-point #'sly-symbol-at-point))
;;     (apply fn args)))


;;; lua

;;;; fennel

(after! fennel-mode
  (put 'unless 'fennel-indent-function 1)
  (put 'hs.hotkey.bind 'fennel-indent-function 'defun)
  (put 'bind 'fennel-indent-function 'defun))

;;; python
(after! python
  (setq! python-fill-docstring-style 'django))

(add-to-list 'interpreter-mode-alist
             (cons (rx bos "uv" eos) #'python-mode))

;;; racket
;; This has precedence over the file extension (#'auto-afadsfm) magic-fallback-mode-alist.
;; Also see `racket-hash-lang-mode'.
(add-to-list 'magic-mode-alist `(,(rx buffer-start "#lang ") . racket-mode))
;; (add-to-list 'magic-mode-alist
;;              (cons (rx buffer-start
;;                        (* (or (any " \t\r\n")         ;spaces
;;                               (: ";" (* not-newline)) ;line comments
;;                               (: "#|"                 ;block comments
;;                                  (* (or (not (any "|#"))
;;                                         (: (+ "#") (not "|"))
;;                                         (: (+ "|") (not "#"))))
;;                                  (* "|")
;;                                  "|#")))
;;                        (or "#lang "))
;;                            ;; (: "(module" (any " \t\r\n"))))
;;                    #'racket-mode))

(use-package! racket-mode
  :config
  (map! :mode racket-mode
        ;; "s-r" #'racket-run-and-switch-to-repl
        "s-e" #'akn/racket-toggle-repl
        "s-i" #'+format/region-or-buffer)
  (defun akn/racket-toggle-repl ()
    (interactive nil racket-mode racket-hash-lang-mode)
    (let ((w (akn/buffer-visible-p "*Racket REPL </>*")))
      (if w
          (delete-window w)
        (racket-repl))))
  (defun insert-lozenge ()
    (interactive nil racket-mode racket-hash-lang-mode)
    (insert "◊"))
  (map! :map racket-mode-map
        "s-L" #'insert-lozenge)

  (add-hook! 'racket-xp-mode-hook
    (defun akn/racket-update-after-parinfer-h (&rest _)
      (if racket-xp-mode
          ;; right after `parinfer-rust-execute' (which is at depth 90)
          (add-hook!  'post-command-hook :local :depth 91 #'akn/racket-update-after-parinfer--post-command-h)
        (remove-hook! 'post-command-hook :local :depth 91 #'akn/racket-update-after-parinfer--post-command-h))))
  (defun akn/racket-update-after-parinfer--post-command-h (&rest _)
    (racket--xp-after-change-hook nil nil nil))

  (add-to-list '+lookup-provider-url-alist
               '("Racket Manuals" "https://docs.racket-lang.org/search/index.html?q=%s"))

  (add-hook 'racket-hash-lang-mode-hook (akn/mode-disabler #'smartparens-mode)))

;; (defun akn/racket-docs-local (query)
;;   (message "%s" query)
;;   (call-process "raco" nil "*raco-docs*" nil
;;                 "docs" (or query "")))

;; (add-to-list '+lookup-provider-url-alist
;;              '("Racket Manuals (local)" akn/racket-docs-local)))

;;; raku
(add-hook 'raku-mode-hook (akn/mode-disabler #'auto-composition-mode))

;;; rocq/coq/proof-general
(after! (:or proof-config proof-useropts)
  (setq! proof-electric-terminator-enable t
         proof-next-command-insert-space t
         proof-autosend-enable nil
         proof-autosend-delay 0.8
         proof-imenu-enable t
         proof-keep-response-history t
         proof-minibuffer-messages t
         proof-full-annotation t
         proof-output-tooltips t
         proof-query-file-save-when-activating-scripting t
         proof-sticky-errors t
         proof-script-fly-past-comments t))

;;; ruby
(after! inf-ruby
  (when (modulep! :editor evil)
    (add-to-list 'evil-insert-state-modes 'inf-ruby-mode)))

;;; rust
;; https://github.com/doomemacs/doomemacs/issues/7588
;; https://github.com/radian-software/apheleia/issues/278
;; see `apheleia-formatters'
(set-formatter! 'rustfmt
  '("rustfmt"
    "--quiet"
    "--emit" "stdout"
    (ignore-errors
      (when-let* ((res
                   (with-temp-buffer
                     (and (equal (call-process "cargo" nil t nil "metadata" "--format-version=1" "--no-deps" "--frozen")
                                 0)
                          (progn (goto-char (point-min))
                                 (json-parse-buffer)))))
                  (packages (gethash "packages" res))
                  (first (seq-first packages))
                  (edition (gethash "edition" first))
                  ((string-match-p (rx bos digit digit digit digit eos) edition)))
        (list "--edition" edition)))))

(add-hook 'rust-mode-hook #'akn/format-on-save-mode)

;;;; rust eglot
(after! eglot
  (when (modulep! :tools lsp +eglot)
    (setf (alist-get 'rustic-mode eglot-server-programs)
          '(eglot-rust-analyzer "rust-analyzer"
            :initializationOptions
            (:check (:command "clippy")
             :rust-analyzer
             (:inlayHints
              (:bindingModeHints (:enable t))                     ;Whether to show inlay type hints for binding modes.
              (:chainingHints (:enable t))                        ;Whether to show inlay type hints for method chains.
              (:closingBraceHints (:enable t                      ;Whether to show inlay hints after a closing } to indicate what item it belongs to.
                                   :minLines 25))                 ;Minimum number of lines required before the } until the hint is shown (set to 0 or 1 to always show them).
              (:closureCaptureHints (:enable t))                  ;Whether to show inlay hints for closure captures.
              (:closureReturnTypeHints (:enable "always"))        ;Whether to show inlay type hints for return types of closures.
              (:closureStyle "impl_fn")                           ;Closure notation in type and chaining inlay hints.
              (:discriminantHints (:enable "always"))             ;Whether to show enum variant discriminant hints.
              (:expressionAdjustmentHints (:enable "always"       ;Whether to show inlay hints for type adjustments.
                                           :hideOutsideUnsafe nil ;Whether to hide inlay hints for type adjustments outside of unsafe blocks.
                                           :mode "prefix"))       ;Whether to show inlay hints as postfix ops (. instead of , etc).
              (:implicitDrops (:enable t))                        ;Whether to show implicit drop hints.
              (:lifetimeElisionHints (:enable "skip_trivial"      ;Whether to show inlay type hints for elided lifetimes in function signatures.
                                      :useParameterNames nil))    ;Whether to prefer using parameter names as the name for elided lifetime hints if possible.
              (:maxLength 25)                                     ;Maximum length for inlay hints. Set to null to have an unlimited length.
              (:parameterHints (:enable t))                       ;Whether to show function parameter name inlay hints at the call site.
              (:rangeExclusiveHints (:enable t))                  ;Whether to show exclusive range inlay hints.
              (:renderColons t)                                   ;Whether to render leading colons for type hints, and trailing colons for parameter hints.
              (:typeHints (:enable t                              ;Whether to show inlay type hints for variables.
                           :hideClosureInitialization nil         ;Whether to hide inlay type hints for let statements that initialize to a closure. Only applies to closures with blocks, same as rust-analyzer.inlayHints.closureReturnTypeHints.enable.
                           :hideNamedConstructor nil))))))))      ;Whether to hide inlay type hints for constructors.

;;; sh-mode
;; for oh-my-zsh
(add-to-list 'auto-mode-alist `(,(rx ".zsh-theme" eos) . sh-mode))

(after! sh-script
  (define-advice dtrt-indent-mode (:around (oldfun &rest args) akn/sh-fix-indentation-a)
    (if (and (derived-mode-p 'sh-mode) dtrt-indent-run-after-smie)
        (akn/letf! ((#'smie-config-guess #'ignore))
          (apply oldfun args))
      (apply oldfun args))))

(defvar akn/original-auto-mode-interpreter-regexp auto-mode-interpreter-regexp)
(setq!
 auto-mode-interpreter-regexp
 (rx (or (regexp akn/original-auto-mode-interpreter-regexp)
         ;; guess shell based on shellcheck directive at top of file
         (seq
          (* "\n") (* (* blank) "#" (* (not ?\n)) (+ "\n"))
          (* blank)
          "#" (* blank) "shellcheck" (+ blank) "shell=" (* blank)
          (group-n 2 (+ (not (any "\t\n "))))))))

(after! sh-script
  (pushnew! (cddr (alist-get 'zsh sh-builtins))
            "zstyle"
            ;; somem others from `man zshbuiltins':
            "cap" "clone" "command" "comparguments" "compcall" "compdescribe"
            "compfiles" "compgroups" "compquote" "comptags" "comptry"
            "compvalues" "echoti" "emulate" "float" "getcap" "job" "last"
            "nocorrect" "noglob" "printf" "pushln" "setcap" "stat" "test" "trap"
            "where" "zcompile" "zformat" "zftp" "zle" "zmodload" "zparseopts"
            "zprof" "zpty" "zregexparse" "zsocket" "ztcp")

  (setq! sh-basic-offset 2))

;;; terminals
;;;; General
(defun akn/recenter-top ()
  (interactive)
  (recenter 0))
(defun akn/recenter-bottom ()
  (interactive)
  (recenter -1))

(map! :map (comint-mode-map mistty-mode-map eat-mode-map)
      "C-l" #'akn/recenter-top)

(defalias 'akn/end-of-buffer-p #'eobp)

(defun akn/terminal-buffer-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'term-mode 'shell-mode 'vterm-mode 'coterm-mode 'mistty-mode 'eshell-mode 'eat-mode)))

(use-package! boring-processes
  :defer-incrementally t
  :init
  (advice-add #'save-buffers-kill-emacs :around #'boring-processes-mark-safe-advice)
  (advice-add #'+workspace/kill :around #'boring-processes-mark-safe-advice)

  (defalias 'akn/kill-current-buffer #'boring-processes-kill-current-buffer)
  (map! [remap kill-current-buffer] #'boring-processes-kill-current-buffer))

;;;; vterm
(defadvice! akn/secret-paste-minibuffer-a (&rest _)
  :before #'comint-send-invisible
  :before #'read-passwd
  (add-transient-hook! 'minibuffer-setup-hook (akn/secret-paste-mode))
  (add-transient-hook! 'minibuffer-exit-hook (akn/secret-paste-mode -1)))
(define-minor-mode akn/secret-paste-mode
  "When pasting, don't add to the kill ring."
  :group 'akn
  :keymap (make-sparse-keymap)
  (akn/mode-set akn/secret-paste-mode
    save-interprogram-paste-before-kill nil))
(map! :map akn/secret-paste-mode-map
      [remap yank] #'akn/secret-paste)
(defun akn/clear-clipboard (&optional text)
  (akn/kill-ring-remove text)
  (let ((select-enable-clipboard t)
        (select-enable-primary t))
    (ignore-errors (funcall interprogram-cut-function ""))
    (ignore-errors (gui-select-text ""))))
(defun akn/kill-ring-remove (text)
  "Remove TEXT from `kill-ring'.

Copied from `embark-kill-ring-remove'."
  (when text
    (unless (eq minibuffer-history-variable t)
      (set minibuffer-history-variable
           (delete text (symbol-value minibuffer-history-variable))))
    (setq kill-ring (delete text kill-ring))))
(defun akn/secret-paste (&optional arg)
  "Like `yank', but then removes it from the clipboard and the kill ring."
  (interactive "*P")
  (setq yank-window-start (window-start))
  (setq this-command t)
  (push-mark)
  ;; CHANGED
  (let ((akn/current-kill (current-kill (cond
                                         ((listp arg) 0)
                                         ((eq arg '-) -2)
                                         (t (1- arg))))))
    (unwind-protect
        (insert-for-yank akn/current-kill)
      (akn/clear-clipboard akn/current-kill)))
  (if (consp arg)
      (goto-char (prog1 (mark t)
                   (set-marker (mark-marker) (point) (current-buffer)))))
  (if (eq this-command t)
      (setq this-command 'yank))
  nil)

(use-package! vterm
  :preface
  ;; COPIED FROM DOOM:
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :init
  (doom-load-packages-incrementally
   '(term/xterm subr-x find-func cl-lib term color compile face-remap tramp bookmark))
  :config
  (map!
   (:map vterm-mode-map
    ;; :gie "C-q" #'vterm-send-next-key
    :gie "C-q" #'vterm--self-insert
    :gie "M-\\" #'vterm-send-next-key
    (:when (modulep! :editor evil)
      :gie "C-g" #'evil-normal-state)
    :gien "C-d" #'vterm--self-insert
    :gien "S-<tab>" #'vterm--self-insert
    ;; :gien "C-c" #'vterm--self-insert
    :gie "M-<backspace>" #'vterm--self-insert
    :gie "S-<return>" #'vterm--self-insert
    :gie "C-j" #'vterm--self-insert
    :gien "M-:" #'eval-expression
    :gie "M-<right>"     (cmd! (vterm-send-key (kbd "C-f")))
    :gie "M-<left>"      (cmd! (vterm-send-key (kbd "C-b")))
    :gie "s-<backspace>" (cmd! (vterm-send-key (kbd "C-u")))
    :gie "s-<left>"      (cmd! (vterm-send-key (kbd "C-a")))
    :gie "s-<right>"     (cmd! (vterm-send-key (kbd "C-e")))
    :gie "<escape>" (cmd! (if (not akn/vterm-escape-mode)
                              (akn/vterm-escape-mode 1)
                            (akn/vterm-escape-mode -1)
                            (vterm-send-key "a")
                            (doom/escape)))
    :gie "SPC"      (cmd! (if (not akn/vterm-escape-mode)
                              (vterm-send-key "SPC")
                            (vterm-send-key "a")
                            (akn/leader))))
   ;; https://github.com/akermu/emacs-libvterm/issues/518
   :gie "C-c p" (akn/defun akn/vterm-send-password ()
                  (interactive nil vterm-mode)
                  (comint-send-invisible "Enter password: ")
                  (vterm-send-string "\n")
                  (clear-this-command-keys)))

  ;; disable ligatures
  (set-ligatures! 'vterm-mode nil)

  (defun akn/vterm-after-send-key (&rest _)
    (akn/vterm-escape-mode -1))

  (add-hook! 'vterm-mode-hook #'akn/vterm-strict-mouse-mode)

  (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

  (when (modulep! :editor evil)
    (add-to-list 'evil-insert-state-modes 'vterm-mode))

  (defvar akn/vterm-escape-mode-map (make-sparse-keymap))
  (define-minor-mode akn/vterm-escape-mode
    "Use the space key as the Emacs leader instead of sending it to vterm."
    :group 'akn
    :init-value nil
    (cond (akn/vterm-escape-mode
           (vterm-send-key "<escape>")
           (advice-add 'vterm-send-key :before #'akn/vterm-after-send-key))
          (t
           (advice-remove 'vterm-send-key #'akn/vterm-after-send-key))))

  (define-minor-mode akn/vterm-strict-mouse-mode
    "When clicking on a vterm, immediately move the cursor back to where it
belongs according to the underlying terminal."
    :group 'akn
    :init-value nil
    ;; TODO: improve this to not use advice like I did with mistty
    (when akn/vterm-strict-mouse-mode
      (defadvice! akn/vterm-reset-cursor-point-a (&rest _args)
        :after 'mouse-set-point
        (when akn/vterm-strict-mouse-mode
          (when (not vterm-copy-mode)
            (call-interactively #'vterm-reset-cursor-point)))))))
;; (message (if (equal (selected-window) (get-buffer-window (current-buffer)))
;;              "in window"
;;            "not in window"))))

;; must declare before loading vterm
(defvar vterm-buffer-name)
(defun noct-run-with-vterm (command)
  "Run COMMAND in a vterm buffer.
Open the vterm buffer reusing a window."
  (interactive
   (list
    (read-shell-command "vterm command: "
                        nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))))
  (let ((vterm-buffer-name (format "*vterm command*"))) ;(car (split-string command))
    (vterm)
    (vterm-insert command)
    (with-current-buffer vterm-buffer-name
      (akn/popup-buffer (akn/this-buffer))
      (evil-insert 1))
    (vterm-send-return)))

(set-popup-rule! (rx "*Install vterm*")
  :vslot -5 :size 0.25 :select nil :modeline nil :quit t :ttl 120)

;;;; term
(after! tramp
  (setq! tramp-terminal-prompt-regexp
         (rx (| (: "TERM = (" (* nonl) ")")
                (: "Terminal type? [" (* nonl) "]"))
             (* blank))))
(map! :after term
      :mode term-mode
      :nie "C-d" #'term-send-eof)
(after! term
  (defvar-local akn/term-command nil)
  (defadvice! akn/set-term-command-a (buffer _name command &rest _)
    :after #'term-exec
    (akn/after-timer! (0)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local akn/term-command command)
          (when (string-match-p (rx "/" (or "zsh" "bash" "fish" "sh" "dash" "tcsh") eos) akn/term-command)
            (setq-local term-prompt-regexp
                        (rx bol
                            (* (not (any "#$%>»❯▶❮V" "\\n")))
                            (+ (any "#$%>»❯▶❮V"))
                            (* " ")))))))))
;; (add-hook 'term-mode-hook #'term-line-mode)
;; (defadvice! akn/term-no-insert-char-mode-a (&rest _)
;;   :override #'evil-collection-term-switch-to-char-mode-on-insert
;;   :override #'evil-collection-term-char-mode-entry-function
;;   :override #'evil-collection-term-sync-state-and-mode
;;   :override #'evil-collection-term-char-mode-insert
;;   nil))
;; (undefadvice! akn/emacs-state-a (fn &rest args)
;;   :around #'term-previous-prompt
;;   :around #'term-next-prompt
;;   :around #'term-bol
;;   :around #'term-kill-input
;;   :around #'term-next-input
;;   :around #'term-previous-input
;;   (evil-execute-in-emacs-state))
;; (defun akn/emacs-state-runner (fn)
;;   (lambda () (interactive) (evil-with-state 'emacs (call-interactively fn))))
;; (defalias 'akn/term-previous-prompt (akn/emacs-state-runner #'term-previous-prompt))
;; (defalias 'akn/term-next-prompt (akn/emacs-state-runner #'term-next-prompt))
;; (defalias 'akn/term-bol (akn/emacs-state-runner #'term-bol))
;; (defalias 'akn/term-kill-input (akn/emacs-state-runner #'term-kill-input))
;; (defalias 'akn/term-next-input (akn/emacs-state-runner #'term-next-input))
;; (defalias 'akn/term-previous-input (akn/emacs-state-runner #'term-previous-input)))
;;;; comint (including shell-mode)
;; NOTE: In shell mode, use comint-previous-prompt (C-c C-p), not term-previous-prompt

(after! comint
  (setq! comint-input-ring-size 3000
         comint-input-ignoredups t
         comint-pager "cat"
         comint-input-autoexpand t
         comint-scroll-to-bottom-on-input t
         comint-insert-previous-argument-from-end t)

  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Shell-echo.html
  (setq-hook! 'shell-mode-hook
    comint-process-echoes t)

  (map! :map comint-mode-map
        :nie "C-d" #'comint-send-eof
        :n "RET" #'comint-send-input
        :i "<up>"   (akn/cmds! (and-let* (((comint-after-pmark-p))
                                          (process (get-buffer-process (current-buffer)))
                                          (pmark (process-mark process))
                                          (pmark-pos (marker-position pmark))
                                          ((>= (point) pmark-pos))
                                          (pmark-bol (save-excursion (goto-char pmark-pos) (pos-bol)))
                                          ((equal (pos-bol) pmark-bol)))
                                #'comint-previous-input))
        :i "<down>" (akn/cmds! (and-let* (((comint-after-pmark-p))
                                          (eob-bol (save-excursion (goto-char (point-max)) (pos-bol)))
                                          ((eq (pos-bol) eob-bol))))
                               #'comint-next-input)
        :v "d" #'comint-kill-region
        :gie "DEL" (cmds! (use-region-p) #'comint-kill-region)
        "M-." #'comint-insert-previous-argument
        :gie "C-r" #'consult-history))

;;;;; coterm
;; This is a global mode that applies to all comint buffers.
;; (coterm-mode)

;;;; eshell

(map! :after eshell
      :map eshell-mode-map
      :nvieomrg "C-d" (akn/cmds! (get-buffer-process (akn/this-buffer)) #'eshell-send-eof-to-process
                                 #'+eshell/kill-and-close))

(use-package! eshell-vterm
  :after eshell
  :config
  (eshell-vterm-mode))

(after! eshell
  (pushnew! eshell-modules-list
            'eshell-elecslash
            'eshell-rebind
            ;; 'eshell-smart
            'eshell-xtra)

  ;; Now we can type "v irb" to run irb in visual mode, for example.
  ;; (And we've set up visual mode to use vterm.)
  (defalias 'eshell/vterm #'eshell-exec-visual)
  (defalias 'eshell/v #'eshell-exec-visual)
  (defadvice! akn/+eshell/kill-and-close-a (fn &rest args)
    :around #'+eshell/kill-and-close
    (let ((is-popup (+popup-window-p)))
      (set-buffer-modified-p nil)
      (with-window-non-dedicated (akn/this-window)
        (apply fn args))
      (when is-popup
        (delete-window))))

  (setq eshell-history-append t))

;; https://www.emacswiki.org/emacs/EshellAlias
(add-hook! 'eshell-alias-load-hook
  (defun akn/eshell-load-aliases ()
    "Read zsh/bash aliases and add them to the list of eshell aliases."
    (interactive)
    (with-timeout (3)
      (let ((default-directory "~/"))
        (with-temp-buffer
          (if (executable-find "zsh")
              (call-process "zsh" nil '(t nil) nil "-ci" "alias -L")
            (call-process "bash" nil '(t nil) nil "-ci" "alias"))
          (goto-char (point-min))
          (while (re-search-forward (rx bol
                                        "alias "
                                        (or (seq "'" (group-n 1 (+ (not (any "'\n")))) "'")
                                            (group-n 1 (+ (not (any "' \n")))))
                                        "="
                                        (seq "'" (group-n 2 (+ (not (any "'\n")))) "'")
                                        eol)
                                    nil t)
            (eshell/alias (match-string 1) (match-string 2))))))))

;;; vdiff
(use-package! vdiff
  :config
  (map! (:map vdiff-mode-map
              "C-c" vdiff-mode-prefix-map
              "s-?" #'vdiff-hydra/body))
  (add-hook! '(vdiff-mode-on-hook vdiff-3way-mode-on-hook)
             #'vdiff-hydra/body))

;; also see https://github.com/justbur/emacs-vdiff-magit

;;; web
(after! web-mode
  (map! :map web-mode-map
        ;; also see https://blog.binchen.org/posts/navigateselect-html-tags-in-emacs.html
        :nvieomrg "C-t" #'web-mode-tag-match
        (:localleader
         "%" #'web-mode-tag-match
         "5" #'web-mode-tag-match))

  ;; (set-eglot-client! '((typescript-tsx-mode :language-id "typescriptreact")) '("typescript-language-server" "--stdio"))
  (derived-mode-add-parents 'typescript-tsx-mode '(tsx-ts-mode))

  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list
                 '(typescript-tsx-mode
                   javascript
                   (typescript-indent-level
                    web-mode-css-indent-offset
                    web-mode-sql-indent-offset
                    web-mode-attr-indent-offset
                    web-mode-code-indent-offset
                    web-mode-markup-indent-offset
                    web-mode-attr-value-indent-offset
                    web-mode-markup-comment-indent-offset)))))

;;; file-local variables

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
