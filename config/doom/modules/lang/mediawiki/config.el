;;; lang/mediawiki/config.el -*- lexical-binding: t; -*-

(require 'akn)

;; missing autoload
(autoload #'mediawiki-edit "mediawiki-page")

(setq! mediawiki-debug t)

(setq! mediawiki-site-alist
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
  revert-buffer-function #'+mediawiki/reload)
(when (fboundp 'better-backup-buffer-mode)
  (add-hook 'mediawiki-mode-hook #'better-backup-buffer-mode))

(map! :after mediawiki-mode
      :map mediawiki-mode-map
      [remap mediawiki-open] #'+mediawiki/open
      [remap mediawiki-open-page-at-point] #'+mediawiki/open-page-at-point
      :mn "RET" (akn/cmds! (+mediawiki-page-at-point) #'+mediawiki/open-page-at-point)
      [remap save-buffer] (akn/cmds! (not (derived-mode-p #'mediawiki-file-mode)) #'mediawiki-save)
      "M-RET" #'mediawiki-terminate-paragraph
      "M-<return>" #'mediawiki-terminate-paragraph
      "s-S" #'mediawiki-save-as
      "s-b" (akn/cmds! (use-region-p) #'mediawiki-insert-bold)
      :i "s-b" #'mediawiki-insert-bold
      "s-i" (akn/cmds! (use-region-p) #'mediawiki-insert-italics)
      :i "s-i" #'mediawiki-insert-italics
      "C-c C-l" #'+mediawiki/insert-link)

;; TODO: article name completion, async with consult (maybe use consult-omni-wikipedia?)

;; TODO: snippets

(after! savehist
  (add-to-list 'savehist-additional-variables 'mediawiki-page-history))

(define-advice mediawiki-add-page-history (:before-while (sitename title) +mediawiki--no-repeats-a)
  (not
   (and-let* ((hist (cdr (assoc-string sitename mediawiki-page-history))))
     (equal title (car hist)))))

(define-advice mediawiki-pagelist-find-page (:around (fn pagelist title &rest args) +mediawiki--normalized-title-a)
  (when-let* ( (normalized (assq 'normalized pagelist))
               (normalized-body (cddr normalized))
               (n (assq 'n normalized-body))
               (n-attrs (cadr n))
               ((equal title (alist-get 'from n-attrs)))
               (normalized-title (alist-get 'to n-attrs)))
    (setq title normalized-title))
  (apply fn pagelist title args))
