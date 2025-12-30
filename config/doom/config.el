;;; $DOOMDIR/config.el - Main Doom Emacs Configuration -*- lexical-binding: t; -*-

;; NOTE: Machine-specific settings should be placed in local.el (gitignored).

(when init-file-debug
  (when (fboundp 'benchmark-init/activate)
    (require 'benchmark-init)
    ;; (setq doom-incremental-first-idle-timer nil)
    ;; (setq akn/incremental-time-rest nil)
    (add-hook 'doom-first-input-hook #'benchmark-init/deactivate)))

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

;; https://discourse.doomemacs.org/t/how-to-have-tool-bar-mode-0-apply-at-startup-to-avoid-large-title-bar-on-macos-sonoma-when-using-railwaycat-homebrew-emacsmacport/4222/2
(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

;; undisable customize stuff
(after! (:or cus-edit doom-ui)
  (dolist (sym '(customize-option customize-browse customize-group customize-face
                 customize-rogue customize-saved customize-apropos
                 customize-changed customize-unsaved customize-variable
                 customize-set-value customize-customized customize-set-variable
                 customize-apropos-faces customize-save-variable
                 customize-apropos-groups customize-apropos-options
                 customize-changed-options customize-save-customized))
    (put sym 'disabled nil)))

(after! gv
  ;; TODO: remove once Emacs un-obsoletes this
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-08/msg02409.html
  (put 'buffer-local-value 'byte-obsolete-generalized-variable nil))

(require 'akn)

;;; init perf stuff
;; load incrementally even if in daemon mode
(setq akn/incremental-time-rest 0.414213
      doom-incremental-first-idle-timer 1.854099
      doom-incremental-idle-timer 0.618033)

;;; Basics

;;;; PATH
;;

;; CLI tools installed by Mise
;; See: https://www.emacswiki.org/emacs/ExecPath
(let* ((shims-path (concat (getenv "HOME") "/.local/share/mise/shims"))
       (with-colon (concat shims-path ":")))
  ;; add shims to front of PATH (if it's not already there at the front)
  (when (not (string-prefix-p with-colon (getenv "PATH")))
    (setenv "PATH" (concat with-colon (getenv "PATH"))))
  ;; add shims to exec-path
  (when (not (equal (car exec-path) shims-path))
    (setq! exec-path (cons shims-path exec-path)))
  ;; remove any other Mise paths from exec-path
  (setq! exec-path
         (seq-filter (lambda (p) (not (string-match-p "local/share/mise/installs" p)))
                     exec-path)))

(when (or (daemonp)
          (member (getenv "TERM_PROGRAM") '("WezTerm" "ghostty")))
  (setq! etcc-term-type-override 'kitty))
;; (setenv "TERM_PROGRAM" "Apple_Terminal")
;; (setenv "TERM_PROGRAM" "")

;;;; Name and email
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.

(setq user-full-name "Ajai Khatri Nelson"
      user-mail-address "ajai@ajai.dev"
      ;; epa-file-encrypt-to "22969541+AjaiKN@users.noreply.github.com")
      epa-file-encrypt-to "ajai@ajai.dev")

;;;; Font
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq! doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' (SPC h r f) to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; (setq doom-font (font-spec :family "Monaco" :weight 'medium :size 12))
;; (setq doom-font (font-spec :family "M PLUS Code Latin" :weight 'medium :size 12))
;; using Iosevka Term because otherwise the ellipsis has width 2
(setq doom-font (font-spec :family "Iosevka Term" :weight 'medium :size 12))
;; (setq doom-variable-pitch-font (font-spec :family "ETBembo" :size 15)) ;ET Book (Edward Tufte)
;; (setq doom-variable-pitch-font (font-spec :family "Times New Roman" :size 15))
;; (setq doom-variable-pitch-font (font-spec :family "Linux Libertine Initials" :size 15))
(setq doom-variable-pitch-font (font-spec :family "Kefa" :size 15))
(setq +zen-text-scale 2)
(setq writeroom-width 80)
(setq doom-font-increment 1) ;smaller increment when pressing zooming in and out
(setq text-scale-mode-step (* doom-font-increment 0.1))

(define-advice doom-init-fonts-h (:before (&rest _) akn/ensure-exist-a)
  (unless (or (not doom-font) (doom-font-exists-p doom-font))
    (message "Font %S not available" doom-font)
    (setq doom-font nil))
  (unless (or (not doom-variable-pitch-font) (doom-font-exists-p doom-variable-pitch-font))
    (message "Font %S not available" doom-variable-pitch-font)
    (setq doom-variable-pitch-font nil)))

(setq akn/original-font doom-font)
(setq akn/original-variable-pitch-font doom-variable-pitch-font)

;; (map! "M-s-+" #'akn/increase-buffer-font-size
;;       "M-s-_" #'akn/decrease-buffer-font-size)

;; ;; based on `text-scale-increase', `doom/increase-font-size', and `doom-adjust-font-size'
;; (defun akn/adjust-buffer-font-size (increment)
;;   (let* ((step (* increment doom-font-increment))
;;          (orig-size (if text-scale-mode text-scale-mode-amount 0))
;;          (new-size (if (= increment 0) 0 (+ orig-size step))))
;;     (when (or (> new-size (text-scale-max-amount))
;;               (< new-size (text-scale-min-amount)))
;;       (user-error "Cannot %s the font size any further"
;;                   (if (> increment 0) "increase" "decrease")))
;;     (setq text-scale-mode-amount new-size))
;;   (text-scale-mode (if (zerop text-scale-mode-amount) -1 1)))
;; (defun akn/increase-buffer-font-size (count &optional increment)
;;   "Enlargens the font size in the current buffer."
;;   (interactive "p")
;;   (akn/adjust-buffer-font-size (* count (or increment doom-font-increment))))
;; (defun akn/decrease-buffer-font-size (count &optional increment)
;;   "Shrinks the font size in the current buffer."
;;   (interactive "p")
;;   (akn/increase-buffer-font-size (- count) increment))

;;;; Theme

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme (cond
                  ((and akn/terminal-daemon-p (modulep! :ui doom))
                   ;; broken on daemon in terminal: doom-ephemeral, doom-plain-dark
                   (setq consult-themes '(default doom-nord doom-rouge doom-xcode doom-opera doom-badger doom-horizon doom-peacock doom-manegarm doom-material doom-flatwhite doom-laserwave doom-palenight doom-spacegrey doom-nord-light doom-city-lights doom-fairy-floss "doom-monokai" doom-nerd-aurora doom-opera-light doom-tokyo-night doom-homage-black doom-homage-white doom-oceanic-next doom-material-dark doom-tomorrow-night "nano"))
                   'doom-opera)
                  (akn/launched-from-terminal-p
                   (fset #'solaire-mode #'ignore)
                   (fset #'solaire-global-mode #'ignore)
                   (fset #'turn-on-solaire-mode #'ignore)
                   (fset #'turn-off-solaire-mode #'ignore)
                   (fset #'solaire-mode-reset #'ignore)
                   nil)
                  ((modulep! :ui doom) 'doom-one)))
;; (setq! doom-theme 'doom-vibrant)

;; (when (equal (daemonp) "term"))
;;   (pushnew! default-frame-alist '(background-color . unspecified))
;;   (set-face-background 'default 'unspecified)
;;   (custom-set-faces! '(default :background nil))
;;   (add-hook! 'doom-load-theme-hook
;;     (set-face-background 'default 'unspecified)
;;     (set-background-color nil)
;;     (custom-set-faces! '(default :background nil))))

;; transparency
(setq frame-alpha-lower-limit 40)

;; (add-to-list 'default-frame-alist '(alpha            . 100))
;; (add-to-list 'default-frame-alist '(alpha-background . 100))

;;;; Other stuff
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; Use `doom/toggle-line-numbers' (SPC t l) to toggle.
;; NOTE: Line numbers can be slow in Emacs
;; - https://github.com/hlissner/.config/doom/blob/6af0a541e0b6b6ec9aee4cb9f05e5cbec0800d91/config.el#L19-L21
;; - https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83/3
(setq-default display-line-numbers-type nil)

;; TODO: PR: make it buffer-local
(make-variable-buffer-local 'doom--line-number-style)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;;; Stuff

(defmacro akn/load-private! (file)
  `(akn/load! ,file akn/private-doom-dir 'noerror))

(akn/load! "+el-patch")

(akn/load! "+completion")

(akn/load! "+buffers")

(akn/load! "+vc")

(akn/load! "+general")

(akn/load-private! "+email")

(akn/load-private! "+project-modes")

(after! avy
  (akn/load! "+avy"))

(when (modulep! :editor evil)
  (akn/load! "+evil"))

(when (modulep! :ui smooth-scroll)
  (akn/load! "+scroll"))

(akn/load! "+major-modes")

(akn/load-private! "+prodigy")

(use-package! org
  :when (modulep! :lang org)
  :defer t
  :commands (akn/open-agenda-file akn/agenda-and-all-todos akn/org-super-agenda)
  :init
  (map! (:leader
         "n A" #'akn/agenda-and-all-todos
         "A"   #'akn/agenda-and-all-todos
         "o a c" #'akn/org-super-agenda
         :desc "todo file" "o a f" #'akn/open-agenda-file))
  :config
  (akn/load-private! "+org"))

(akn/load-private! "+private")

;;; init perf stuff
;; (not sure whether the daemonp part is necessary)
(unless noninteractive
  (akn/incrementally! (:append t)
    (when               doom-first-input-hook
      (doom-log "akn: Running doom-first-input-hook early")
      (doom-run-hooks  'doom-first-input-hook)
      (setq akn/doom-first-input-hook-ran doom-first-input-hook) ; just to make it easier to look later at what hooks were run
      (setq             doom-first-input-hook nil)))
  (akn/incrementally! (:append t)
    (when               doom-first-buffer-hook
      (doom-log "akn: Running doom-first-buffer-hook early")
      (doom-run-hooks  'doom-first-buffer-hook)
      (setq akn/doom-first-buffer-hook-ran doom-first-buffer-hook)
      (setq             doom-first-buffer-hook nil)))
  (akn/incrementally! (:append t)
    (when               doom-first-file-hook
      (doom-log "akn: Running doom-first-file-hook early")
      (doom-run-hooks  'doom-first-file-hook)
      (setq akn/doom-first-file-hook-ran doom-first-file-hook)
      (setq             doom-first-file-hook nil))))

;; trampver.el is very slow to load! It calls `emacs-repository-get-version' while calculating `tramp-repository-version'.
(akn/incrementally! ()
  (unwind-protect
      (akn/letf! ((advice-add #'emacs-repository-get-branch :override #'ignore)
                  (advice-add #'emacs-repository-get-version :override #'ignore))
        (require 'trampver nil t))
    (akn/doom-prioritize-load-packages-incrementally
     '(tramp-compat tramp-integration cl-lib tramp-loaddefs tramp files))
    (defvar akn/trampver-fully-loaded nil)
    (defun akn/fully-load-trampver ()
      (unless akn/trampver-fully-loaded
        (setq akn/trampver-fully-loaded (load "trampver" nil 'nomessage))))
    (unless akn/trampver-fully-loaded
      (akn/after-idle! ((* 60 4)) (akn/fully-load-trampver))
      (add-transient-hook! #'tramp-debug-message (akn/fully-load-trampver))
      (add-transient-hook! #'tramp-bug (akn/fully-load-trampver)))))

(doom-load-packages-incrementally `(hideshow
                                    tree-widget timer recentf
                                    savehist
                                    saveplace
                                    server
                                    better-jumper
                                    dtrt-indent
                                    so-long
                                    ws-butler
                                    cl-lib timer filenotify autorevert
                                    dash thingatpt help-mode loadhist smartparens
                                    tsc-dyn tsc map seq tree-sitter-cli tree-sitter-load tree-sitter
                                    cl-lib font-lock seq treesit
                                    dash s format elisp-refs help help-mode radix-tree help-fns f find-func nadvice info info-look backtrace macroexp debug edebug trace imenu cc-langs helpful
                                    dired-loaddefs dired dirvish dired-x cond-let eieio transient dirvish-collapse dirvish-vc dirvish-subtree all-the-icons vscode-icon dirvish-icons dirvish-side
                                    dash treemacs-macros treemacs-customization treemacs-logging treemacs-core-utils treemacs-scope treemacs-themes treemacs-icons treemacs-faces treemacs-visuals treemacs-async  treemacs-rendering treemacs-follow-mode treemacs-filewatch-mode treemacs-mode treemacs-interface  treemacs-compatibility treemacs-workspaces treemacs-fringe-indicator treemacs-header-line treemacs-annotations ;; treemacs-persistence treemacs
                                    ;; evil treemacs-evil ;; projectile treemacs-projectile magit pfuture seq treemacs-magit persp-mode eieio dash treemacs-persp
                                    seq subr-x s ts-fold-util ts-fold-parsers ts-fold-summary ts-fold
                                    js2-mode newcomment sgml-mode rjsx-mode typescript-mode json ;; npm-mode
                                    etags json cl-lib eldoc dash s flycheck imenu thingatpt tide-lv tabulated-list xref ; tide ; TypeScript Interactive Development Environment for Emacs
                                    git-auto-commit-mode

                                    seq esh-util esh-module pcomplete esh-arg esh-io esh-proc esh-opt esh-ext eldoc generator esh-cmd esh-var esh-mode eshell ring em-dirs eshell-z em-unix env dash ansi-color man esh-help eshell-did-you-mean em-alias text-property-search em-prompt em-dirs eshell-syntax-highlighting em-cmpl fish-completion em-tramp em-banner em-basic em-dirs em-extpipe em-glob em-hist em-ls em-pred em-script em-term em-elecslash em-rebind em-smart em-xtra

                                    ;; lsp-mode
                                    ,@(and (modulep! :tools lsp -eglot)
                                           '(cl-generic cl-lib compile dash epg-config epg rfc6068 ewoc f filenotify files gv ht imenu macroexp inline json lv markdown-mode auth-source nsm puny network-stream pcase rx s seq spinner subr-x wid-edit tree-widget url-vars auth-source url-parse url-util ring project widget xref minibuffer help-mode lsp-protocol)) ; lsp-mode
                                    ;; eglot
                                    ,@(and (modulep! :tools lsp +eglot)
                                           '(imenu cl-lib url-parse url-util pcase compile icons warnings filenotify cl-lib debug backtrace ewoc find-func pp map ert text-property-search easy-mmode diff-mode diff eglot))

                                    ;; racket
                                    rx sh-script comint racket-custom
                                    subr-x racket-util
                                    gv cl-macs cl-lib filenotify tramp racket-back-end
                                    tramp racket-cmd racket-browse-url
                                    cl-extra thingatpt tramp racket-custom racket-keywords-and-builtins racket-ppss racket-font-lock racket-indent racket-parens racket-util racket-common
                                    url-util racket-doc racket-describe shr url-util racket-scribble racket-complete racket-describe racket-doc racket-eldoc racket-custom cl-macs face-remap racket-show racket-visit ansi-color compile easymenu rx xref semantic semantic/symref grep semantic/symref/grep ring racket-repl
                                    cl-macs hideshow racket-edit
                                    imenu racket-imenu
                                    racket-doc racket-describe racket-eldoc racket-imenu racket-visit racket-show racket-xp-complete easymenu imenu rx seq xref racket-xp
                                    racket-parens racket-ppss racket-smart-open
                                    racket-profile
                                    easymenu rx racket-logger
                                    shell term racket-shell
                                    easymenu rx racket-stepper
                                    tramp racket-repl-buffer-name
                                    tq racket-collection
                                    lisp-mode racket-lisp-mode
                                    cl-macs cus-edit package seq racket-back-end racket-bug-report
                                    racket-mode

                                    org-appear))

(defvar akn/funcs-to-compile
  (list
   #'akn/line-numbers-on-when-narrowing-a
   #'akn/line-numbers-off-when-widening-a
   #'akn/set-jump-before-mwheel-scroll-h
   #'akn/jk-nav-flash-h
   #'akn/completion-add-space-after-point-a
   #'akn/fussy-ignore-spaces-a
   ;; #'akn/winner-save-old-configurations-a
   #'akn/tab-line-undedicate-a
   #'+evil-adjust-shift-width-h@akn/a
   #'akn/scroll-progressively
   #'akn/do-scroll-conservatively
   #'akn/scroll-conservatively
   #'akn/projectile-ignored-projects--faster-file-truename-a
   #'akn/file-remote-p
   #'akn/tramp-p
   #'akn/tramp-home-dir
   #'akn/known-project-roots-above
   #'akn/projectile-root-assume-cached
   #'akn/projectile-invalidate-cached-project-roots
   #'akn/doom-project-ignored-p--a
   #'akn/vertico-sort-directories-first-alpha
   #'+corfu-add-cape-dabbrev-h
   #'+magit-revert-buffer-maybe-h
   #'+doom-dashboard-reload-maybe-h
   #'+file-templates-check-h
   #'doom-auto-revert-buffer-h
   #'doom-auto-revert-buffers-h
   #'doom-run-switch-buffer-hooks-h
   #'doom-run-switch-window-hooks-h
   #'doom--recentf-touch-buffer-h
   #'+popup-buffer-p
   #'akn/terminal-buffer-p
   #'+tabs-maybe-add-tab-line-for-popup-buffer
   #'+vc-gutter-use-margins-in-tty-h
   #'doom-auto-revert-buffer-h
   #'+vc-gutter-update-h
   #'akn/magit-wip--only-if-not-tramp-a
   #'consult-project-buffer@akn/a
   #'count-words--format@akn/add-paragraphs-a
   #'doom-temp-buffer-p@akn/polymode-a
   #'fasd-add-file-to-db@akn/fix-new-file-a
   #'goto-addr-mode--turn-on@akn/a
   #'parinfer-rust--check-for-issues@akn/a
   #'polymode-eval-buffer@akn/redisplay-a
   #'akn/add-tramp-home-dir-to-stop-dir-a
   #'project-try-vc@akn/add-tramp-home-dir-to-stop-dir-a
   #'projectile-ignored-projects@akn/fast-a
   #'projectile-project-root@akn/tramp-speed-up-a
   #'projectile-root-marked@akn/a
   #'save-some-buffers@akn/preview-a
   #'save-some-buffers@akn/preview-a2
   #'sp-backward-sexp@akn/a
   #'sp-forward-sexp@akn/a
   ;; #'akn/disable-slow-adaptive-fill-functions-a
   #'akn/lsp-booster--advice-json-parse
   #'akn/lsp-booster--advice-final-command
   #'akn/xterm-color--advice-compilation-filter-a
   #'akn/better-backup-buffer-mode-maybe-h
   #'akn/better-jumper-set-jump-a
   #'akn/+emacs-lisp-truncate-pin
   #'akn/fix-ws-butler-in-evil-mode/before-save-a
   #'akn/fix-ws-butler-in-evil-mode/after-save-a))
(akn/after-idle! ((* 60 12) :timer-name akn/func-compile-timer :cancel-old-timer t)
  (akn/after-idle! (10 :repeat t :timer-name akn/func-compile-timer2 :cancel-old-timer t)
    (if-let* ((fn (pop akn/funcs-to-compile)))
        (dlet ((shut-up-ignore doom-debug-mode))
          (doom-log "compile-functions: %s" fn)
          (shut-up
            (let (byte-compile-warnings)
              (or (if (featurep 'native-compile)
                      (or (native-comp-function-p (indirect-function fn))
                          (ignore-errors (native-compile fn))))
                  (byte-code-function-p fn)
                  (byte-compile fn)))))
      (cancel-timer akn/func-compile-timer))))


;; packages loaded on first input hook
;; (cl-callf2 append
;;     '(cl-lib evil-easymotion
;;       orderless)
;;     doom-incremental-packages)

;; also see optimization for projectile above

;;; file-local variables

;; disable make-local warnings (Doom already disables the other 3: see `+emacs-lisp-linter-warnings')

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
