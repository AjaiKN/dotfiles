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

;; TODO: savehist for mediawiki-page-history
