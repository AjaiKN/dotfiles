;;; emacs/casual/config.el -*- lexical-binding: t; -*-

(map! (:after isearch
       :map isearch-mode-map
       "s-?" #'casual-isearch-tmenu)
      (:after evil-easymotion
       :m "g S" #'casual-avy-tmenu
       :map evilem-map ; "g s"
       "s-?" #'casual-avy-tmenu
       "m"   #'casual-avy-tmenu
       ";"   #'casual-avy-tmenu)
      (:after dired
       :map (dired-mode-map wdired-mode-map)
       "s-?"        #'casual-dired-tmenu)
      (:after ibuffer
       :map ibuffer-mode-map
       ;; "F" #'casual-ibuffer-filter-tmenu
       ;; "s" #'casual-ibuffer-sortby-tmenu
       ;; "<double-mouse-1>" #'ibuffer-visit-buffer ; optional
       ;; "M-<double-mouse-1>" #'ibuffer-visit-buffer-other-window ; optional
       ;; "{" #'ibuffer-backwards-next-marked ; optional
       ;; "}" #'ibuffer-forward-next-marked   ; optional
       ;; "[" #'ibuffer-backward-filter-group ; optional
       ;; "]" #'ibuffer-forward-filter-group  ; optional
       ;; "$" #'ibuffer-toggle-filter-group  ; optional
       "s-?" #'casual-ibuffer-tmenu)
      (:after re-builder
       :map reb-mode-map      "s-?" #'casual-re-builder-tmenu
       :map reb-lisp-mode-map "s-?" #'casual-re-builder-tmenu)
      (:after bookmark
       :map bookmark-bmenu-mode-map
       ;; "S" #'casual-bookmarks-sortby-tmenu
       "s-?" #'casual-bookmarks-tmenu)
      (:after info
       :map Info-mode-map
       "s-?" #'casual-info-tmenu)
      (:after (:or org-agenda evil-org-agenda)
       :map (org-agenda-mode-map evil-org-agenda-mode-map)
       "s-?" #'casual-agenda-tmenu))

(use-package! casual-isearch
  :commands (casual-isearch-tmenu))

(after! casual-dired
  (transient-suffix-put #'casual-dired-tmenu #'dired-maybe-insert-subdir :key "I")
  (transient-remove-suffix #'casual-dired-tmenu #'casual-dired-image-info)
  (transient-suffix-put #'casual-dired-tmenu #'dired-previous-line :key "k")
  (transient-suffix-put #'casual-dired-tmenu #'dired-next-line :key "j")
  (transient-suffix-put #'casual-dired-tmenu #'bookmark-jump :key "SPC RET")
  (transient-suffix-put #'casual-dired-tmenu #'dired-find-file-other-window :key "S-<return>")
  (transient-replace-suffix #'casual-dired-tmenu (if (ignore-errors (transient-get-suffix #'casual-dired-tmenu #'wdired-change-to-wdired-mode))
                                                     #'wdired-change-to-wdired-mode
                                                   #'dired-toggle-read-only)
    '("i" "Edit (wdired)" dired-toggle-read-only :if-mode dired-mode :transient nil))
  (transient-define-prefix akn/transient-dired-mark ()
    [["Mark"
      ("c" "Change marks" dired-change-marks :transient nil)
      ("C-n" "Next marked file" dired-next-marked-file :transient nil)
      ("C-p" "Previous marked file" dired-prev-marked-file :transient nil)
      ("t" "Toggle marks" dired-toggle-marks :transient nil)]
     ["Mark objects"
      ("*" "executables" dired-mark-executables :transient nil)
      ("/" "directories" dired-mark-directories :transient nil)
      ("@" "symlinks" dired-mark-symlinks :transient nil)
      ("%" "regexp" dired-mark-files-regexp :transient nil)
      ("s" "subdir files" dired-mark-subdir-files :transient nil)
      ("m" "this file" dired-mark :transient nil)]
     ["Unmark"
      ("u" "this file" dired-unmark :transient nil)
      ("DEL" "Unmark backwards" dired-unmark-backward :transient nil)
      ("?" "all files" dired-unmark-all-files :transient nil)
      ("!" "all marks" dired-unmark-all-marks :transient nil)
      ("U" "all marks" dired-unmark-all-marks :transient nil)]])
  (transient-define-prefix akn/transient-dired-regex ()
    [["Marking/Flagging"
      ("d" "Flag files for deletion by regexp" dired-flag-files-regexp)
      ("m" "Mark files matching regexp" dired-mark-files-regexp)
      ("g" "Mark files with *contents* containing regexp" dired-mark-files-containing-regexp)
      ("&" "Flag garbage files for deletion" dired-flag-garbage-files)]
     ["Operations"
      ("r" "Rename by regexp" dired-do-rename-regexp)
      ("C" "Copy by regexp" dired-do-copy-regexp)
      ("H" "Hardlink by regexp" dired-do-hardlink-regexp)
      ("S" "Symlink by regexp" dired-do-symlink-regexp)
      ("R" "Rename by regexp" dired-do-rename-regexp)]
     ["Case"
      ("u" "Rename marked files to uppercase" dired-upcase)
      ("l" "Rename marked files to lowercase" dired-downcase)]])

  (transient-append-suffix #'casual-dired-tmenu #'dired-mark
    '("*" "Mark›" akn/transient-dired-mark :transient nil))
  (transient-append-suffix #'casual-dired-tmenu "q"
    '("%" "Regexp›" akn/transient-dired-mark :transient nil)))

(after! casual-ibuffer
  :config
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-backward-line :key "k")
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-forward-line :key "j")
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-jump-to-buffer :key "J")
  (transient-suffix-put #'casual-ibuffer-tmenu #'bookmark-jump :key "C-<return>")
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-update :key "g r")
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-visit-buffer-other-window :key "g o")
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-copy-filename-as-kill :key "y f")
  (transient-suffix-put #'casual-ibuffer-tmenu #'ibuffer-bury-buffer :key "X"))

(after! casual-info
  :config
  (transient-suffix-put #'casual-info-tmenu #'Info-menu :key "J")
  (transient-suffix-put #'casual-info-tmenu #'casual-info-browse-backward-paragraph :key "{")
  (transient-suffix-put #'casual-info-tmenu #'casual-info-browse-forward-paragraph :key "}")
  (transient-suffix-put #'casual-info-tmenu #'Info-goto-node :key "g G")
  (transient-suffix-put #'casual-info-tmenu #'Info-toc :key "g T")
  (transient-suffix-put #'casual-info-tmenu #'Info-history :key "g L")
  (transient-suffix-put #'casual-info-tmenu #'Info-prev-reference :key "g [")
  (transient-suffix-put #'casual-info-tmenu #'Info-next-reference :key "g ]")
  (transient-suffix-put #'casual-info-tmenu #'Info-backward-node :key "C-k")
  (transient-suffix-put #'casual-info-tmenu #'Info-forward-node :key "C-j")
  (transient-suffix-put #'casual-info-tmenu #'Info-prev :key "g k")
  (transient-suffix-put #'casual-info-tmenu #'Info-next :key "g j")
  (transient-suffix-put #'casual-info-tmenu #'bookmark-jump :key "M-SPC RET")
  (transient-suffix-put #'casual-info-tmenu #'Info-goto-node-web :key "g w"))

(after! casual-agenda
  ;; free up j and k for evil
  (transient-suffix-put #'casual-agenda-tmenu #'org-agenda-goto-date :key "J")
  (transient-suffix-put #'casual-agenda-operations-tmenu #'org-agenda-goto-date :key "J")
  (transient-suffix-put #'casual-agenda-tmenu #'org-capture :key "K"))

(after! casual-symbol-overlay
  (when (fboundp 'symbol-overlay-mc-insert-into-casual-tmenu)
    (symbol-overlay-mc-insert-into-casual-tmenu)))
