;;; emacs/repeat/config.el -*- lexical-binding: t; -*-

;; I only want "SPC g [" and "SPC g ]" to be repeatable.
;; I don't want "[ d" and "] d" to be repeatable.
(defalias '+repeat/+vc-gutter/previous-hunk #'+vc-gutter/previous-hunk)
(defalias '+repeat/+vc-gutter/next-hunk     #'+vc-gutter/next-hunk)

(use-package! repeat-help
  :defer-incrementally (which-key repeat)
  :ghook 'repeat-mode-hook
  :config
  (define-advice repeat-help-mode (:before (&rest _) +repeat)
    "First remove the repeat-help hooks, in case we've already activated
`repeat-help-mode', but `repeat-help-auto' has changed since then."
    (advice-remove 'repeat-post-hook #'repeat-help--activate)
    (advice-remove 'repeat-post-hook #'repeat-help--activate-auto))

  (setq repeat-help-auto nil
        ;; 'embark, 'which-key, or t
        repeat-help-popup-type 'which-key)
  (repeat-help-mode)
  (unless repeat-help-auto
    (setq! repeat-echo-function #'repeat-echo-message)))

;; or https://tildegit.org/acdw/define-repeat-map.el/src/branch/main/define-repeat-map.el
(use-package! repeaters
  :defer-incrementally (repeat)
  :after-call repeat-mode-hook
  :config
  (repeaters-define-maps
   '(
     ;; ("akn/buffer-switch"
     ;;  previous-buffer                    "C-x C-<left>" "C-x <left>" "C-<left>" "<left>" "p"
     ;;  next-buffer                        "C-x C-<right>" "C-x <right>" "C-<right>" "<right>" "n"
     ;;  evil-switch-to-windows-last-buffer "l" "o")
     ("akn/winner"
      winner-undo                       "<left>" "u" "C-u"
      winner-redo                       "<right>" "r" "C-r")
     ("akn/workspace-swap"
      +workspace/swap-left    "<left>"
      +workspace/swap-right   "<right>"
      +workspace/display      "TAB")
     ;; +workspace/switch-left  "[" :exitonly
     ;; +workspace/switch-right "]" :exitonly
     ;; +workspace/switch-to-0  "0" :exitonly
     ;; +workspace/switch-to-1  "1" :exitonly
     ;; +workspace/switch-to-2  "2" :exitonly
     ;; +workspace/switch-to-3  "3" :exitonly
     ;; +workspace/switch-to-4  "4" :exitonly
     ;; +workspace/switch-to-5  "5" :exitonly
     ;; +workspace/switch-to-6  "6" :exitonly
     ;; +workspace/switch-to-7  "7" :exitonly
     ;; +workspace/switch-to-8  "8" :exitonly
     ;; +workspace/switch-to-9  "9" :exitonly
     ;; +workspace/kill         "d" :exitonly
     ;; +workspace/rename       "r" :exitonly
     ;; +workspace/save         "s" :exitonly
     ;; +workspace/new          "n" :exitonly
     ;; +workspace/new-named    "N" :exitonly))))
     ("akn/zoom"
      doom/reset-font-size    "s-0" "0"
      doom/decrease-font-size "s--" "-"
      doom/increase-font-size "s-=" "=" "+")
     ("akn/git-stage-hunk"
      akn/stage-hunk             "s"
      akn/unstage-hunk           "u"
      +repeat/+vc-gutter/previous-hunk  "["
      +repeat/+vc-gutter/next-hunk      "]"
      magit-file-stage          "S"
      magit-file-unstage        "U"
      magit-commit              "c" :exitonly)
     ("akn/ligatures"
      prettify-symbols-mode    "e"
      auto-composition-mode    "f"
      akn/toggle-all-ligatures  "l"
      akn/toggle-all-ligatures  "L")
     ("akn/line-numbers"
      doom/toggle-line-numbers "l")
     ("akn/tabs"
      global-tab-line-mode     "t"
      tab-line-mode            "T")
     ("akn/visible"
      visible-mode             "v")
     ("akn/wrap"
      +word-wrap-mode          "w"
      +global-word-wrap-mode   "W")
     ("akn/zen"
      +zen/toggle              "z")
     ("akn/opacity"
      akn/toggle-opacity                     "o"
      akn/set-frame-opacity/alpha-background "O"
      akn/opacity-up                         "<up>" "k"
      akn/opacity-down                       "<down>" "j")
     ("akn/errors"
      flycheck-next-error             "n"
      flycheck-previous-error         "p" "N"
      flycheck-explain-error-at-point "e")
     ("akn/comint-C-C-M-r"
      comint-previous-matching-input-from-input "M-r" "r"
      comint-next-matching-input-from-input "M-s" "s")
     ("akn/tab-bar-toggle-show"
      +tab-bar/toggle-show "t" "C-t")
     ("akn/smerge"
      smerge-next              "n" "j" "<down>"
      smerge-prev              "p" "k" "<up>"
      smerge-resolve           "r"
      smerge-keep-all          "a"
      smerge-keep-base         "b"
      smerge-keep-lower        "o"
      smerge-keep-lower        "l"
      smerge-keep-upper        "m"
      smerge-keep-upper        "u"
      smerge-ediff             "E" :exitonly
      smerge-combine-with-next "C"
      smerge-refine            "R"
      smerge-keep-current      "C-m"
      smerge-diff-base-upper   "= <" "<"
      smerge-diff-base-lower   "= >" ">"
      smerge-diff-upper-lower  "= =")
     ("akn/treemacs-filewatch-mode"             treemacs-filewatch-mode             "a")
     ("akn/treemacs-indicate-top-scroll-mode"   treemacs-indicate-top-scroll-mode   "c")
     ("akn/treemacs-git-commit-diff-mode"       treemacs-git-commit-diff-mode       "d")
     ("akn/treemacs-follow-mode"                treemacs-follow-mode                "f")
     ("akn/treemacs-git-mode"                   treemacs-git-mode                   "g")
     ("akn/treemacs-toggle-show-dotfiles"       treemacs-toggle-show-dotfiles       "h")
     ("akn/treemacs-hide-gitignored-files-mode" treemacs-hide-gitignored-files-mode "i")
     ("akn/treemacs-indent-guide-mode"          treemacs-indent-guide-mode          "n")
     ("akn/treemacs-fringe-indicator-mode"      treemacs-fringe-indicator-mode      "v")
     ("akn/treemacs-toggle-fixed-width"         treemacs-toggle-fixed-width         "w"))))

(use-package! shut-up :defer-incrementally t)

(use-package! repeat
  :after-call doom-first-input-hook
  :defer-incrementally t
  :config
  (setq! repeat-exit-key "<escape>"
         repeat-exit-timeout (* 60 3))

  ;; ;; not sure why the regular `repeat-exit' function isn't working for me
  (defun akn/repeat-exit ()
    (when repeat-in-progress
      ;; https://lists.nongnu.org/archive/html/emacs-devel/2022-09/msg00678.html
      (cl-callf append unread-command-events (listify-key-sequence (kbd repeat-exit-key)))
      t))
  (add-function :after after-focus-change-function
                #'akn/repeat-exit)

  (after! dired
    (function-put #'dired-jump 'repeat-map nil))

  (require 'repeaters)

  (when (not repeat-mode)
    (shut-up (repeat-mode 1))))
