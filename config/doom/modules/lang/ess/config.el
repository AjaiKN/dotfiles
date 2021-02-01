;;; lang/ess/config.el -*- lexical-binding: t; -*-

(require 'akn)

(after! projectile
  (pushnew! projectile-project-root-files "DESCRIPTION" ".here")
  (pushnew! projectile-project-root-files-top-down-recurring "remake.yml"))


;;; ESS

(use-package! ess
  :defer t
  :init
  (unless (modulep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.[jJ][lL]\\'" . ess-julia-mode)))
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake (or (not (modulep! :checkers syntax))
                            (modulep! :checkers syntax +flymake))
        ess-nuke-trailing-whitespace-p t
        ess-style 'RStudio ;other notable options: DEFAULT, OWN, RRR, RStudio-
        ess-history-directory (expand-file-name "ess-history/" doom-cache-dir)

        ess-ask-for-ess-directory nil
        ess-startup-directory-function
        (lambda ()
          (when-let* ((start (or buffer-file-name default-directory)))
            ;; https://here.r-lib.org/reference/here.html
            (locate-dominating-file
             start
             (lambda (dir)
               (when (file-directory-p dir)
                 (let ((default-directory dir))
                   (or (file-exists-p ".here")
                       (ignore-errors (doom-glob "*.Rproj"))
                       (file-exists-p "DESCRIPTION")
                       (file-exists-p "remake.yml")
                       (file-exists-p ".projectile") (file-exists-p ".project")
                       (file-exists-p ".git")
                       (file-exists-p ".svn"))))))))

        ess-use-R-completion (not (or (modulep! :completion ido) (modulep! :completion company)))
        ess-use-ido (modulep! :completion ido)
        ess-use-company (modulep! :completion company)

        ess-eval-visibly nil)

  (after! ess-r-mode
    ;; start comments with # instead of ##
    (setf (alist-get 'comment-add ess-r-customize-alist) 0))

  (add-hook 'inferior-ess-mode-hook #'doom-mark-buffer-as-real-h)

  (set-docsets! 'ess-r-mode "R")
  (when (modulep! +lsp)
    (add-hook 'ess-r-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'ess-r-mode-local-vars-hook #'tree-sitter! 'append))

  (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl)
  (set-repl-handler! 'ess-julia-mode #'+ess/open-julia-repl)
  (set-lookup-handlers! '(ess-r-mode ess-julia-mode inferior-ess-mode inferior-ess-r-mode)
    :documentation #'ess-display-help-on-object)

  (set-evil-initial-state! 'ess-r-help-mode 'normal)

  (set-eval-handler! '(ess-mode ess-r-mode ess-bugs-mode ess-help-mode
                       ess-jags-mode ess-gretl-mode ess-julia-mode
                       ess-watch-mode ess-r-help-mode ess-rdired-mode
                       ess-rutils-mode ess-view-data-mode ess-transcript-mode
                       ess-r-transcript-mode ess-r-package-menu-mode)
    #'+ess-eval)

  ;; can re-enable this on a per-project basis
  (after! flycheck
    (pushnew! (default-value 'flycheck-disabled-checkers) 'r-lintr))

  (set-company-backend! '(ess-r-mode inferior-ess-r-mode)
    '(company-R-args company-R-objects company-dabbrev-code :separate))

  (setq-hook! 'ess-r-mode-hook
    ;; previously `ess-newline-and-indent' (also see `ess-roxy-newline')
    ;; HACK Fix #2233: Doom continues comments on RET, but ess-r-mode doesn't
    ;;      have a sane `comment-line-break-function', so...
    comment-line-break-function nil)
  ;; (define-advice ess-roxy-newline (:around (fn &optional _use-soft-newlines) +ess)
  ;;   (let ((comment-line-break-function nil))
  ;;     (funcall fn)))
  ;; (define-advice ess-newline-and-indent (:around (fn &rest args) +ess)
  ;;   (let ((comment-line-break-function nil))
  ;;     (apply fn args)))

  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(ess-mode c/c++/java ess-indent-offset)))
  (after! editorconfig
    (add-to-list 'editorconfig-indentation-alist '(ess-mode ess-indent-offset)))
  (after! lsp-mode
    (add-to-list 'lsp--formatting-indent-alist '(ess-mode . ess-indent-offset)))

  ;; HACK: make the REPL buffer more responsive.
  (setq-hook! 'inferior-ess-mode-hook
    comint-scroll-to-bottom-on-input t
    comint-scroll-to-bottom-on-output t
    comint-move-point-for-output t))

(map! (:after ess-help
       (:map ess-help-mode-map
        :n "q"  #'kill-current-buffer
        :n "Q"  #'ess-kill-buffer-and-go
        :n "K"  #'ess-display-help-on-object
        :n "go" #'ess-display-help-in-browser
        :n "gO" #'ess-display-help-apropos
        :n "gv" #'ess-display-vignettes
        :m "]]" #'ess-skip-to-next-section
        :m "[[" #'ess-skip-to-previous-section)
       (:map ess-doc-map
        "h"    #'ess-display-help-on-object
        "p"    #'ess-view-data-print
        "P"    #'ess-R-dv-pprint
        "T"    #'ess-R-dv-ctable
        [C-return] #'ess-eval-line
        :nvieomrg "s-<return>" #'ess-eval-region-or-line-and-step))
      (:after ess-mode
       :map ess-mode-map
       :n [C-return] #'ess-eval-line
       :nvieomrg "s-<return>" #'ess-eval-region-or-line-and-step
       :localleader
       "," #'ess-eval-region-or-function-or-paragraph-and-step
       "'" #'R
       [tab]     #'ess-switch-to-inferior-or-script-buffer
       [backtab] #'ess-switch-process
       ;; REPL
       "B" #'ess-eval-buffer-and-go
       "b" #'ess-eval-buffer
       "d" #'ess-eval-region-or-line-and-step
       "D" #'ess-eval-function-or-paragraph-and-step
       "L" #'ess-eval-line-and-go
       "l" #'ess-eval-line
       "R" #'ess-eval-region-and-go
       "r" #'ess-eval-region
       "F" #'ess-eval-function-and-go
       "f" #'ess-eval-function
       ;; predefined keymaps
       "h" 'ess-doc-map
       "x" 'ess-extra-map
       "p" 'ess-r-package-dev-map
       "v" 'ess-dev-map
       ;; noweb
       :prefix "c"
       "C" #'ess-eval-chunk-and-go
       "c" #'ess-eval-chunk
       "d" #'ess-eval-chunk-and-step
       "m" #'ess-noweb-mark-chunk
       "p" #'ess-noweb-previous-chunk
       "n" #'ess-noweb-next-chunk)
      (:after ess-roxy
       :map ess-roxy-mode-map
       :i "RET" #'ess-indent-new-comment-line
       :nviemorg [remap back-to-indentation] #'ess-roxy-goto-end-of-roxy-comment
       :nviemorg [remap newline] #'ess-roxy-newline
       :nviemorg [remap newline-and-indent] #'ess-roxy-newline
       :nviemorg [remap ess-indent-command] #'ess-roxy-ess-indent-command
       :nviemorg [remap move-beginning-of-line] #'ess-roxy-move-beginning-of-line
       :nviemorg [remap beginning-of-visual-line] #'ess-roxy-move-beginning-of-line)
      (:after ess-r-mode
       :map (ess-r-mode-map inferior-ess-r-mode-map)
       :ierg "s-M" #'+ess/insert-pipe)
      (:after (:or ess-mode ess-inf)
       :map (ess-mode-map inferior-ess-mode-map)
       :nviemorg "s-)" #'inferior-ess-reload
       (:localleader "s" #'ess/setup-windows)))

;; https://github.com/emacs-ess/ESS/issues/1267
;; https://github.com/emacs-ess/ESS/issues/1297
;; https://github.com/minad/corfu/issues/234#issuecomment-1262912749
;; https://github.com/minad/corfu/issues/229
;; https://github.com/emacs-ess/ESS/issues/1226
(after! ess
  (dolist (capf (list #'ess-roxy-complete-tag
                      #'ess-filename-completion
                      #'ess-r-package-completion
                      #'ess-r-object-completion))
    ;; https://github.com/minad/cape?tab=readme-ov-file#capf-transformers
    (advice-add capf :around #'cape-wrap-noninterruptible)
    (advice-add capf :around #'cape-wrap-purify)
    (advice-add capf :around #'cape-wrap-debug))

  ;; If the above doesn't stop the process from crashing, try this:
  ;; (setq-hook! '(ess-mode-hook inferior-ess-mode-hook)
  ;;   corfu-auto nil)
  nil)

(after! ess
  ;; C-c C-o C-o
  (setq! ess-roxy-template-alist
         (list (cons "description" "")
               ;(cons "details" ".. content for \\details{} ..")
               ;(cons "title" "")
               (cons "param" "")
               (cons "return" "")
               ;(cons "author" ess-user-full-name)
               (cons "examples" ""))))

;;;; Data viewing

(use-package! ess-R-data-view
  :defer t
  :init
  (map! :map (ess-r-mode-map inferior-ess-r-mode-map)
        "C-c C-d P" #'ess-R-dv-pprint
        "C-c C-d T" #'ess-R-dv-ctable)
  :config)
(use-package! ess-view-data
  :defer t
  :init
  (map! :map (ess-r-mode-map inferior-ess-r-mode-map)
        "C-c C-d p" #'ess-view-data-print)
  :config)

;;;; Plots
(map! :after (:or ess-mode ess-inf essgd)
      :map (ess-mode-map inferior-ess-mode-map essgd-mode-map)
      :localleader "P" #'+ess/r-plot-open
      :localleader "p" #'+ess/essgd-r-plot-open)

(use-package! essgd
  :defer t
  :config
  (set-evil-initial-state! 'essgd-mode 'motion)
  (map! :map essgd-mode-map
        :nviemg "r" #'essgd-refresh
        :nviemg "p" #'essgd-prev-plot
        :nviemg "n" #'essgd-next-plot
        :nviemg "c" #'essgd-clear-plots
        :nviemg "x" #'essgd-remove-plot-move-next
        :nviemg "<backspace>" #'essgd-remove-plot-move-previous
        :nviemg "q" #'quit-window
        :nviemg "<left>" #'essgd-prev-plot
        :nviemg "<right>" #'essgd-next-plot
        :nviemg "s-<left>" (cmd! (message "First plot") (essgd-show-plot-n 1))
        :nviemg "s-<right>" (cmd! (message "Last plot") (essgd-show-plot-n (length essgd-plot-nums))))

  (add-hook! 'essgd-mode-hook
    (defun +ess--essgd-h ()
      (tab-line-mode -1)
      (hide-mode-line-mode)
      (set-window-dedicated-p (selected-window) 'plot)))

  (akn/advise-letf! essgd-show-plot-n (+ess-set-background-a)
    (define-advice create-image (:filter-return (img) +ess-set-background-a)
      (when (imagep img)
        (setf (image-property img :background) "white"))
      img)))

;;; Stan

(use-package! stan-mode
  :when (modulep! +stan)
  :hook (stan-mode . stan-mode-setup)
  :hook (stan-mode . eldoc-stan-setup)
  :init
  (use-package! company-stan
    :when (modulep! :completion company)
    :hook (stan-mode . company-stan-setup))

  (use-package! flycheck-stan
    :when (modulep! :checkers syntax -flymake)
    :hook (stan-mode . flycheck-stan-stanc2-setup)
    :hook (stan-mode . flycheck-stan-stanc3-setup)))
