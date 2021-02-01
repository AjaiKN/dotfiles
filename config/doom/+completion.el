;;; +completion.el -*- lexical-binding: t; -*-

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

;;; Completion

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; if orderless doesn't find anything, fall back to fuzzy search
;; also see `completion-category-defaults' and `completion-category-overrides'
(defun akn/set-completion-styles ()
  (let ((maybe-fussy (when (modulep! :completion fuzzy) '(fussy))))
    (setq completion-styles `(orderless ,@maybe-fussy basic)) ;akn/emacs22-if-not-minibuffer))
    (setf (alist-get `imenu completion-category-overrides) `((styles akn/orderless-without-initialism orderless ,@maybe-fussy basic)))
    (setf (alist-get `consult-grep completion-category-overrides) `((styles akn/orderless-without-initialism basic)))
    (setf (alist-get `consult-location completion-category-overrides) `((styles akn/orderless-without-initialism basic)))
    (pushnew! completion-category-overrides
              ;; `(lsp-capf         (styles orderless ,@maybe-fussy basic)) ;doom`s default
              `(imenu             (styles akn/orderless-without-initialism orderless ,@maybe-fussy basic))
              `(file              (styles +vertico-basic-remote akn/orderless-without-prefix-dispatcher-or-initialism ,@maybe-fussy partial-completion))
              `(bookmark          (styles +vertico-basic-remote akn/orderless-without-prefix-dispatcher ,@maybe-fussy partial-completion))
              `(consult-location  (styles akn/orderless-without-initialism ,@maybe-fussy basic))
              `(consult-grep      (styles akn/orderless-without-initialism ,@maybe-fussy basic))
              ;; Override the defaults in completion-category-defaults
              `(racket-identifier (styles ,@completion-styles))
              `(racket-module     (styles ,@completion-styles))
              `(email             (styles ,@completion-styles)))))
(akn/set-completion-styles)
(after! (:or orderless vertico)
  (akn/set-completion-styles))

(add-to-list 'completion-styles-alist
             `(akn/emacs22-if-not-minibuffer
               ,(akn/defun akn/emacs22-if-not-minibuffer-try-completion (&rest args)
                  (unless (derived-mode-p 'minibuffer-mode)
                    (apply #'completion-emacs22-try-completion args)))
               ,(akn/defun akn/emacs22-if-not-minibuffer-all-completions (&rest args)
                  (unless (derived-mode-p 'minibuffer-mode)
                    (apply #'completion-emacs22-all-completions args)))
               "Like emacs22, except only applies if we're not in the minibuffer."))
(add-to-list
 'completion-styles-alist
 '(+vertico-basic-remote
   +vertico-basic-remote-try-completion
   +vertico-basic-remote-all-completions
   "Use basic completion on remote files only"))

(use-package! orderless
  :init
  (let ((styles '((akn/orderless-without-initialism        (orderless-matching-styles '(orderless-literal orderless-regexp)))
                  ;; (akn/orderless-with-annotation          (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-annotation)))
                  ;; (akn/orderless-with-flex                (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex)))
                  ;; (akn/orderless-with-flex-and-annotation (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex orderless-annotation)))
                  ;; (akn/orderless-just-annotation          (orderless-matching-styles '(orderless-annotation)))
                  (akn/orderless-without-prefix-dispatcher (orderless-style-dispatchers (remq #'akn/prefixes-for-separators orderless-style-dispatchers)))
                  (akn/orderless-without-prefix-dispatcher-or-initialism
                   (orderless-style-dispatchers (remq #'akn/prefixes-for-separators orderless-style-dispatchers))
                   (orderless-matching-styles '(orderless-literal orderless-regexp))))))
    (dolist (style styles)
      (let* ((style-name (car style))
             (style-try-completion  (akn/symbol-format "%s-try-completion"  style-name))
             (style-all-completions (akn/symbol-format "%s-all-completions" style-name)))
        ;; register the completion style
        (add-to-list 'completion-styles-alist (list style-name style-try-completion style-all-completions nil))
        ;; autoload the completion style
        (when (not (fboundp style-try-completion))  (autoload style-try-completion  "orderless"))
        (when (not (fboundp style-all-completions)) (autoload style-all-completions "orderless"))))
    (after! orderless
      (dolist (style styles)
        ;; actually define the completion style
        (eval `(orderless-define-completion-style ,(car style)
                 ,@(cdr style))
              t))))

  :config
  ;; https://github.com/oantolin/orderless#component-matching-styles
  (setq! orderless-matching-styles (list #'orderless-literal
                                         #'orderless-regexp
                                         #'orderless-initialism))

  ;; add a space after point (to mimic substring completion style)
  (defadvice! akn/completion-add-space-after-point-a (args)
    :filter-args #'orderless-try-completion
    :filter-args #'orderless-all-completions
    (cl-destructuring-bind (string table pred point) args
      (list (if (and (stringp string)
                     (numberp point)
                     (< 1 point (length string))
                     (not (memq (aref string (- point 1))
                                '(?^)))
                     (not (string-match-p (rx (or (seq (any "!%`=~") (* (not space)) eos)
                                                  (seq (any "^") eos)
                                                  (seq "\\" (any "(" "|"))
                                                  (seq "[" (* (not "]")) eos)))
                                          (substring string 0 point))))
                (concat (substring string 0 point)
                        " "
                        (substring string point))
              string)
            table
            pred
            point)))

  ;; https://old.reddit.com/r/emacs/comments/11lyg9k/have_an_emacs_completion_setup_that_works_really/jbf5mqw/
  ;; "For `propertized-buffer-identification', I'd definitely type something
  ;; like `pr-b-id'. That will match with either the `partial-completion'
  ;; completion style or the `orderless-prefixes' matching style in orderless.
  ;; In my personal orderless configuration I have a style dispatcher that will
  ;; use `orderless-prefixes' automatically if a component has hyphens."
  ;; https://github.com/oantolin/emacs-config/blob/a80c3b6a4c7e0fa87254a0c148fe7f9b2976edd1/init.el#L405-L415
  (defun akn/prefixes-for-separators (pattern _index _total)
    (when (string-match-p (rx bol
                              (* (not (any "*+[\\]^"))) ; try to exclude regexps (https://old.reddit.com/r/emacs/comments/11lyg9k/have_an_emacs_completion_setup_that_works_really/jca4e53/)
                              (any "./-")
                              (* (not (any "$*+[\\]")))
                              eol)
                          pattern)
      (cons (list #'orderless-prefixes #'orderless-regexp) pattern)))
  (add-to-list 'orderless-style-dispatchers #'akn/prefixes-for-separators t)

  (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch))

(defun akn/should-tab-definitely-indent-p ()
  (and (not (use-region-p))
       (string-match-p (rx bol (* blank) eos)
                       (buffer-substring (pos-bol) (point)))))

(let ((tab-cmd (akn/cmds! (akn/should-tab-definitely-indent-p)
                          (akn/cmds! (derived-mode-p 'org-mode) #'org-metaright #'evil-shift-right-line)))
      (stab-cmd (akn/cmds! (derived-mode-p 'org-mode) #'org-metaleft #'evil-shift-left-line)))
  (map! :i "TAB"       tab-cmd
        :i "<tab>"     tab-cmd
        :i "<backtab>" stab-cmd
        :i "S-<tab>"   stab-cmd
        :i "S-TAB"     stab-cmd))


(after! corfu
  (map! (:map corfu-map
         ;; when inside popup
         :i "C-SPC" #'corfu-insert-separator
         :gie "<tab>" #'corfu-next
         :gie "TAB" #'corfu-next
         :gie "<backtab>" #'corfu-previous
         :gie "S-TAB" #'corfu-previous)
        (:map corfu-mode-map
         ;; when corfu-mode is on
         :i "C-@"   #'completion-at-point
         :i "C-SPC" #'completion-at-point
         :i "C-n"   #'+corfu/dabbrev-or-next
         :i "C-p"   #'+corfu/dabbrev-or-last
         :nv "C-SPC" nil)
        :i "C-S-SPC" #'set-mark-command
        :i "C-@"     #'set-mark-command))
(setq-default tab-always-indent t)
(add-hook 'corfu-mode-hook #'akn/corfu-tab-stuff-h)
(defun akn/corfu-tab-stuff-h ()
  (if corfu-mode
      (unless (derived-mode-p 'org-mode)
        (setq-local tab-always-indent 'complete))
    (when (eq tab-always-indent 'complete)
      (kill-local-variable 'tab-always-indent))))
(add-hook! '(org-mode-hook markdown-mode-hook)
  (defun akn/corfu-mode-off ()
    (corfu-mode -1)))

;; (use-package! corfu-mouse
;;   :ghook 'corfu-mode-hook)

;;; file-local variables

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved make-local)
;; End:
