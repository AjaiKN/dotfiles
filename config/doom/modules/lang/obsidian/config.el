;;; lang/obsidian/config.el -*- lexical-binding: t; -*-

(add-hook 'markdown-mode-hook #'akn/tab-display2)
(defun akn/tab-display2 ()
  (interactive)
  (setq-local tab-width 2))

;; (after! (:or markdown-mode evil-markdown))
(use-package! markdown-mode
  :defer t
  :defer-incrementally (easymenu outline thingatpt cl-lib url-parse button color rx subr-x))

(use-package! obsidian
  :defer t
  :after-call (markdown-mode-hook evil-markdown-mode)
  :commands (akn/obsidian-vault akn/obsidian-capture)
  :defer-incrementally (f dash s cl-lib markdown-mode yaml)
  :custom
  (obsidian-directory "~/Documents/obsidian-vault")
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "capture")
  ;; Create missing files in inbox? - when clicking on a wiki link
  ;; t: in inbox, nil: next to the file with the link
  ;; default: t
  (obsidian-wiki-link-create-file-in-inbox nil)
  ;; The directory for daily notes (file name is YYYY-MM-DD.md)
  ;;(obsidian-daily-notes-directory "Daily Notes")
  ;; Directory of note templates, unset (nil) by default
  (obsidian-templates-directory "templates")
  ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
  ;;(setq obsidian-daily-note-template "Daily Note Template.md")
  :config
  (defun akn/obsidian-vault ()
    (interactive)
    (require 'projectile)
    (projectile-switch-project-by-name obsidian-directory))
  (when (not global-obsidian-mode)
    (global-obsidian-mode t))
  (map!
   (:map obsidian-mode-map
    :localleader
    "O" #'obsidian-follow-link-at-point
    "B" #'obsidian-backlink-jump
    "W" #'obsidian-insert-wikilink
    "L" #'obsidian-insert-link
    "J" #'obsidian-jump                  ;; Opening a note
    "D" #'obsidian-daily-note         ;; Creating daily note
    "a" #'akn/launch-obsidian-app
    "l" #'akn/launch-obsidian-app))
  (defun akn/launch-obsidian-app ()
    (interactive)
    (shell-command
     (format "open -a Obsidian %s"
             (if (and buffer-file-name (file-in-directory-p buffer-file-name obsidian-directory))
                 (shell-quote-argument buffer-file-name)
               ""))))
  (add-hook 'obsidian-mode-hook #'indent-tabs-mode)

  (defun akn/obsidian-capture ()
    "Create new obsidian note *with the default name as YYYY-MM-DD*.

In the `obsidian-inbox-directory' if set otherwise in `obsidian-directory' root."
    (interactive)
    ;;                                   CHANGED (argument added)
    (let* ((title (read-string "Title: " (format-time-string "%Y-%m-%d")))
                                                      ;; CHANGED
           (filename (s-concat obsidian-directory "/" (or (akn/class-based-on-time) obsidian-inbox-directory) "/" title ".md"))
           (clean-filename (s-replace "//" "/" filename)))
      (find-file (expand-file-name clean-filename) t)
      (save-buffer)
      (add-to-list 'obsidian-files-cache clean-filename)))

  (defun akn/rename-visited-file (new-file-name)
    (interactive
     (list (if buffer-file-name
               ;;                                                     CHANGED (argument added)
               (read-file-name "Rename visited file to: " nil nil nil (file-name-nondirectory buffer-file-name))
             (read-file-name "Set visited file name: "
                             default-directory
                             (expand-file-name
                              (file-name-nondirectory (buffer-name))
                              default-directory)))))
    (rename-visited-file new-file-name)
    (akn/set-base-file-name))
  (static-when (modulep! :ui modeline +light)
    (def-modeline-var! akn/base-file-name nil))
  (defun akn/set-base-file-name () (when (modulep! :ui modeline +light) (setq-local akn/base-file-name (file-name-base buffer-file-name))))
  (def-project-mode! akn/obsidian-header-line-mode
    :modes '(obsidian-mode)
    :on-enter (progn (akn/set-base-file-name)
                     (setq-local header-line-format '(:propertize akn/base-file-name face markdown-header-face-1)))
    :on-exit (setq-local header-line-format nil))
  (map! :map akn/obsidian-header-line-mode-map
        "<header-line> <down-mouse-1>" #'ignore-preserving-kill-region
        "<header-line> <mouse-1>" #'akn/rename-visited-file))
