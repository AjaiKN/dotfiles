;;; lang/mediawiki/config.el -*- lexical-binding: t; -*-

(require 'akn)

;; missing autoload
(autoload #'mediawiki-edit "mediawiki-page")

(setopt mediawiki-debug t)

;; TODO: PR: fix `mediawiki-site-alist' defcustom type for setopt
(setq mediawiki-site-alist
      '(("Wikipedia"
         "https://en.wikipedia.org/w/"
         ""
         ""
         ""
         :description "English Wikipedia"
         :first-page "Main Page")
        ("Wiktionary"
         "https://en.wiktionary.org/w/"
         ""
         ""
         ""
         :description "English Wiktionary"
         :first-page "Main Page")
        ("Wikimedia Commons"
         "https://commons.wikimedia.org/w/"
         ""
         ""
         ""
         :description "Wikimedia Commons"
         :first-page "Main Page")))

(add-hook 'mediawiki-mode-hook #'doom-mark-buffer-as-real-h)
(setq-hook! 'mediawiki-mode-hook
  revert-buffer-function #'+mediawiki-revert-buffer-fn
  adaptive-fill-regexp (rx (* (any " \t:*#"))))
(when (fboundp 'better-backup-buffer-mode)
  (add-hook 'mediawiki-mode-hook #'better-backup-buffer-mode))
(add-hook! 'mediawiki-mode-hook
  (defun +mediawiki--update-tab-line-h ()
    (add-hook! 'after-change-functions :local
      (when (bound-and-true-p tab-line-mode)
        (tab-line-force-update nil)))))

(map! :after mediawiki-mode
      :map mediawiki-mode-map
      [remap mediawiki-open] #'+mediawiki/open
      [remap wikipedia-open] #'+mediawiki/open
      [remap mediawiki-open-page-at-point] #'+mediawiki/open-page-at-point
      :mn "RET" (akn/cmds! (+mediawiki-page-at-point) #'+mediawiki/open-page-at-point)
      [remap mediawiki-save] #'+mediawiki/save
      [remap wikipedia-save] #'+mediawiki/save
      [remap mediawiki-do-login] #'+mediawiki/login
      [remap wikipedia-login] #'+mediawiki/login
      [remap mediawiki-do-logout] #'+mediawiki/logout
      [remap wikipedia-logout] #'+mediawiki/logout
      [remap save-buffer]                   (akn/cmds! (+mediawiki-virtual-p) #'+mediawiki/save)
      [remap browse-url-of-file]            (akn/cmds! (+mediawiki-virtual-p) #'mediawiki-browse)
      [remap +macos/reveal-in-finder]       (akn/cmds! (+mediawiki-virtual-p) #'mediawiki-browse)
      [remap +vc/browse-at-remote]          (akn/cmds! (+mediawiki-virtual-p) #'mediawiki-browse)
      [remap +vc/browse-at-remote-kill]     (akn/cmds! (+mediawiki-virtual-p) (cmd! (kill-new (mediawiki-make-url mediawiki-page-title "view"))))
      [remap +vc/browse-at-remote-homepage] (akn/cmds! (+mediawiki-virtual-p) (cmd! (browse-url (mediawiki-site-url mediawiki-site))))
      [remap +vc/browse-at-remote-homepage] (akn/cmds! (+mediawiki-virtual-p) (cmd! (kill-new (mediawiki-site-url mediawiki-site))))
      [remap diff-buffer-with-file]         (akn/cmds! (+mediawiki-virtual-p) #'mediawiki-diff-to-live)
      [remap magit-log-buffer-file]         (akn/cmds! (+mediawiki-virtual-p) #'+mediawiki/history)
      "M-RET" #'mediawiki-terminate-paragraph
      "M-<return>" #'mediawiki-terminate-paragraph
      "s-S" #'mediawiki-save-as
      "s-b" (akn/cmds! (use-region-p) #'mediawiki-insert-bold)
      :i "s-b" #'mediawiki-insert-bold
      "s-i" (akn/cmds! (use-region-p) #'mediawiki-insert-italics)
      :i "s-i" #'mediawiki-insert-italics
      "C-c C-l" #'+mediawiki/insert-link

      :map mediawiki-history-mode-map
      :mn "RET" #'mediawiki-history-view-revision
      :mn "d"   #'mediawiki-history-diff-to-previous
      :mn "D"   #'mediawiki-history-diff-to-current
      :mn "e"   #'mediawiki-history-diff-revisions
      :mn "R"   #'mediawiki-history-restore-revision
      :mn "o"   #'mediawiki-history-browse-revision
      :mn "c"   #'mediawiki-history-diff-to-current
      :mn "p"   #'mediawiki-history-open-page)

;; TODO: article name completion, async with consult (maybe use consult-omni-wikipedia?)

(after! (:and mediawiki-mode smartparens)
  (setf (alist-get 'mediawiki-mode sp-pairs) nil)
  (dolist (delim '("=" "==" "===" "====" "=====" "======"))
    (sp-local-pair 'mediawiki-mode (concat delim " ") (concat " " delim) :actions '(insert))))

(after! savehist
  (add-to-list 'savehist-additional-variables 'mediawiki-page-history))

(define-advice mediawiki-pagelist-find-page (:around (fn pagelist title &rest args) +mediawiki--normalized-title-a)
  (when-let* ( (normalized (assq 'normalized pagelist))
               (normalized-body (cddr normalized))
               (n (assq 'n normalized-body))
               (n-attrs (cadr n))
               ((equal title (alist-get 'from n-attrs)))
               (normalized-title (alist-get 'to n-attrs)))
    (setq title normalized-title))
  (apply fn pagelist title args))

(akn/advise-letf! mediawiki-pop-to-buffer (+mediawiki--same-window-a)
  (display-buffer-overriding-action (cons #'display-buffer-same-window nil)))
(set-popup-rule! (rx bos "*MW History: ") :ignore t)

(add-hook 'mediawiki-history-mode-hook #'akn/mark-buffer-real)

(define-advice mediawiki-api-call (:filter-args (args) +mediawiki--assert-user-a)
  (cl-destructuring-bind (sitename action &optional params) args
    (when (or (equal action "edit"))
              ;; (and (equal action "query")
              ;;      (member (cons "meta" "tokens") params)
              ;;      (member (cons "type" "csrf") params)))
      (setq params (append params
                           (list
                            (cons "assert" "user")
                            (cons "assertuser" (let ((username (mediawiki-site-username sitename)))
                                                 (when (string-match (rx bos (group (* (not "@"))) "@" (* anything) eos)
                                                                     username)
                                                   (setq username (match-string 1 username)))
                                                 username))))))
    (list sitename action params)))

(akn/pushnew +word-wrap-text-modes #'mediawiki-mode #'mediawiki-file-mode #'mediawiki-draft-mode)

;; (defun akn/syntax-table-set (&rest args)
;;   (let ((table (when (syntax-table-p (car args)) (pop args))))
;;     (while args
;;       (let ((chars (pop args))
;;             (newentry (pop args)))
;;         (mapc (lambda (c)
;;                 (modify-syntax-entry c newentry table))
;;               (if (characterp chars)
;;                   (string chars)
;;                 chars))))))

(after! mediawiki-mode
  (mapc (lambda (x)
            (modify-syntax-entry x "." mediawiki-mode-syntax-table))
        "|*-=:#;'\""))

(define-advice mediawiki-retry-save-page (:around (fn sitename &rest args) +mediawiki--login-a)
  (+mediawiki/login sitename)
  (akn/letf! ((#'mediawiki-do-login #'ignore))
    (apply fn sitename args)))

;; TODO: bug? PR?
(define-advice mediawiki-api-get-revision-content (:filter-args (args) +mediawiki--num-to-string-a)
  (cl-destructuring-bind (sitename title revid &rest rest) args
    (when (numberp revid)
      (setq revid (number-to-string revid)))
    `(,sitename ,title ,revid ,@rest)))

;;;; Wikipedia

(use-package! wikipedia
  :when (modulep! +wikipedia)
  :defer t
  :init
  (define-advice mediawiki-edit (:around (fn sitename &rest args) +mediawiki--wikipedia-edit-mode-a)
    (let ((ret-buffer (apply fn sitename args)))
      (when-let* (((bufferp ret-buffer))
                  (domain (cadr (assoc sitename mediawiki-site-alist)))
                  ((string-match-p "\\<wikipedia.org\\>" domain)))
        (with-current-buffer ret-buffer
          (wikipedia-edit-mode)))
      ret-buffer)))
