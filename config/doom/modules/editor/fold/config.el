;;; editor/fold/config.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'akn-doom-use-package)
  (require 'doom-keybinds))

(defun +fold--to-char (thing)
  (cond
   ((characterp thing) thing)
   ((and (stringp thing) (length= thing 1)) (string-to-char thing))
   (t nil)))

(defun +fold-ellipsis-set (symbol value)
  (set-default-toplevel-value symbol value)
  (when-let* ((v (bound-and-true-p +fold-ellipsis)))
    (set-default-toplevel-value 'org-ellipsis v)
    (set-default-toplevel-value 'ts-fold-replacement v))
  (when (boundp '+fold-truncate-ellipsis)
    (set-default-toplevel-value 'truncate-string-ellipsis (default-toplevel-value '+fold-truncate-ellipsis)))
  (when (boundp 'outline-minor-mode)
    (dolist (buf
             (nconc
              (match-buffers '(derived-mode . outline-mode))
              (match-buffers (lambda (buffer-or-name _arg)
                               (buffer-local-value 'outline-minor-mode (get-buffer buffer-or-name))))))
      (with-current-buffer buf
        (+fold--set-outline-minor-mode-ellipsis)))))

(defcustom +fold-ellipsis " [...] "
  "The ellipsis to show for elided regions (folds).
`org-ellipsis' and `ts-fold-replacement' are set to this."
  :type 'string
  :group '+fold
  :set #'+fold-ellipsis-set)
(defcustom +fold-outline-ellipsis nil
  "The ellipsis to show for elided regions in `outline-minor-mode'.
If nil, the value of `+fold-ellipsis' is used."
  :type 'string
  :group '+fold
  :set #'+fold-ellipsis-set)
(defcustom +fold-truncate-ellipsis "..."
  "The ellipsis to use for `truncate-string-ellipsis'."
  :type 'string
  :group '+fold
  :set #'+fold-ellipsis-set)

(add-hook! 'outline-minor-mode-hook :append
  (defun +fold--set-outline-minor-mode-ellipsis ()
    ;; from `outline-indent--update-ellipsis'
    (when (bound-and-true-p outline-minor-mode)
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c))
                                     (or +fold-outline-ellipsis +fold-ellipsis "...")))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table)))))

(when (modulep! :editor evil)
  ;; Add vimish-fold, outline-mode & hideshow support to folding commands
  (define-key! 'global
    [remap evil-toggle-fold]   #'+fold/toggle
    [remap evil-close-fold]    #'+fold/close
    [remap evil-open-fold]     #'+fold/open
    [remap evil-open-fold-rec] #'+fold/open-rec
    [remap evil-close-folds]   #'+fold/close-all
    [remap evil-open-folds]    #'+fold/open-all)
  (after! evil
    (evil-define-key* 'motion 'global
      "zj" #'+fold/next
      "zk" #'+fold/previous
      "zf" #'evil-vimish-fold/create
      "zF" #'evil-vimish-fold/create-line
      "zd" #'vimish-fold-delete
      "zE" #'vimish-fold-delete-all)))

;;
;;; Packages

(use-package! hideshow ; built-in
  :defer t
  :defer-incrementally t
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-hide-level
             hs-show-all
             hs-hide-all)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'+fold-hideshow-set-up-overlay-fn)

  (defadvice! +fold--hideshow-ensure-mode-a (&rest _)
    "Ensure `hs-minor-mode' is enabled when we need it, no sooner or later."
    :before '(hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
    (+fold--ensure-hideshow-mode))

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                        ""
                        "#"
                        +fold-hideshow-forward-block-by-indent-fn nil)
             (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp-fn nil)
             (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                        "end\\|[]}]"
                        "#\\|=begin"
                        ruby-forward-sexp)
             (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                          "end"
                          nil (lambda (_arg) (matlab-forward-sexp)))
             (nxml-mode "<!--\\|<[^/>]*[^/]>"
                        "-->\\|</[^/>]*[^/]>"
                        "<!--" sgml-skip-tag-forward nil)
             (latex-mode
              ;; LaTeX-find-matching-end needs to be inside the env
              ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
              "\\\\end{[a-zA-Z*]+}"
              "%"
              (lambda (_arg)
                ;; Don't fold whole document, that's useless
                (unless (save-excursion
                          (search-backward "\\begin{document}"
                                           (line-beginning-position) t))
                  (LaTeX-find-matching-end)))
              nil))
           hs-special-modes-alist
           '((t))))))


(use-package! evil-vimish-fold
  :when (modulep! :editor evil)
  :defer t
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
             evil-vimish-fold/delete evil-vimish-fold/delete-all
             evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode +1))

;; Will be autoloaded by fold commands
(use-package! treesit-fold
  :when (modulep! :tools tree-sitter)
  :defer t
  :config (global-treesit-fold-mode +1))

(use-package! ts-fold
  :when (modulep! :tools tree-sitter)
  :after tree-sitter
  :defer t
  :config
  ;; we want to use our own face so we nullify this one to have no effect and
  ;; make it more similar to hideshows
  (custom-set-faces! '(ts-fold-replacement-face :foreground unspecified
                                                :box nil
                                                :inherit font-lock-comment-face
                                                :weight light))
  (setq ts-fold-replacement +fold-ellipsis)
  (global-ts-fold-mode +1))

(after! org
  (setq org-ellipsis +fold-ellipsis))

;;outli mode
;;see speed keys
(use-package! outli
  :defer t
  :defer-incrementally (outline color org-keys)
  ;; `emacs-lisp-mode-hook' isn't strictly necessary here (`outline-minor-mode-hook' is enough),
  ;; since Doom already enables `outline-minor-mode' for elisp, but we might as well add it so
  ;; that `outline-minor-mode' doesn't get called twice (since `outli-mode' calls `outline-minor-mode').
  :hook ((emacs-lisp-mode outline-minor-mode) . +fold/outli-mode)
  :config
  (pushnew! outli-heading-config
            ;;MODE STEM REPEAT-CHAR [STYLE] [NO-BAR]
            '(sh-mode "#" ?\# t)
            '(conf-unix-mode "#" ?\# t))
  (add-hook! '(sh-mode-hook conf-unix-mode-hook)
             #'outli-mode)

  ;; outli-mode-map includes this even for insert mode, but I want it only in normal mode
  (map! :map outli-mode-map
        "<backtab>" nil
        "S-<tab>" nil)
  (defun +fold/outli-mode ()
    (if outline-minor-mode
        ;; `outli-mode' calls `outline-minor-mode', so if we didn't check this first, we'd have infinite recursion
        (unless (or (bound-and-true-p outli-mode)
                    (bound-and-true-p outline-indent-minor-mode)
                    (bound-and-true-p so-long-minor-mode))
          (outli-mode))
      (when (bound-and-true-p outli-mode)
        (outli-mode -1)))))

(use-package! outline-indent
  :defer t)

(use-package! comint-fold
  :after-call comint-mode
  :defer-incrementally (hideshow
                        ring ansi-color ansi-osc regexp-opt comint)
  :config
  (setq! comint-fold-remap-tab nil
         comint-fold-fringe-indicator nil)
  (map! :map comint-mode-map
        :gie "TAB"       (akn/cmds! (not (comint-after-pmark-p)) #'+fold/toggle)
        :gie "<tab>"     (akn/cmds! (not (comint-after-pmark-p)) #'+fold/toggle)
        :gie "<backtab>" (akn/cmds! (not (comint-after-pmark-p)) #'+fold/outline-cycle-all-simple)
        :gie "S-<tab>"   (akn/cmds! (not (comint-after-pmark-p)) #'+fold/outline-cycle-all-simple)
        :gie "S-TAB"     (akn/cmds! (not (comint-after-pmark-p)) #'+fold/outline-cycle-all-simple))
  (comint-fold-mode))
