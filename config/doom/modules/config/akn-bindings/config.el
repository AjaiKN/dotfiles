;;; config/akn-bindings/config.el -*- lexical-binding: t; -*-

(require 'akn)
(require 'general)
(eval-when-compile
  (require 'doom-keybinds))

;;; fullscreen stuff

(unless (fboundp 'akn/fullscreen-toggle)
  (defun akn/fullscreenp ()
    (frame-parameter nil 'fullscreen))
  (defun akn/fullscreen-on ()
    (interactive)
    (when (and (display-graphic-p) (not (akn/fullscreenp)))
      (toggle-frame-fullscreen)
      (akn/transparency-off)))
  (defun akn/fullscreen-off ()
    (interactive)
    (when (and (display-graphic-p) (akn/fullscreenp))
      (toggle-frame-fullscreen)
      (akn/transparency-on)))
  (defun akn/fullscreen-toggle ()
    (interactive)
    (if (akn/fullscreenp)
        (akn/fullscreen-off)
      (akn/fullscreen-on)))
  (add-hook 'doom-after-modules-config-hook #'akn/fullscreen-on))

;;; apply Doom's macos bindings on all OSs
(map! "s-`" #'other-frame  ; fix frame-switching
      ;; fix OS window/frame navigation/manipulation keys
      "s-w" #'delete-window
      "s-W" #'delete-frame
      "s-n" #'+default/new-buffer
      "s-N" #'make-frame
      "s-q" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
      "C-s-f" #'toggle-frame-fullscreen
      ;; Restore somewhat common navigation
      "s-l" #'goto-line
      ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
      ;; it imposes some other functionality and overhead we don't need)
      "s-f" (if (modulep! :completion vertico) #'consult-line #'swiper)
      "s-z" #'undo
      "s-Z" #'redo
      "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
      "s-v" #'yank
      "s-s" #'save-buffer
      "s-x" #'execute-extended-command
      :v "s-x" #'kill-region
      "s-0" #'doom/reset-font-size
      ;; Global font scaling
      "s-=" #'doom/increase-font-size
      "s-+" #'doom/increase-font-size
      "s--" #'doom/decrease-font-size
      "s-_" #'doom/decrease-font-size
      ;; Conventional text-editing keys & motions
      "s-a" #'mark-whole-buffer
      "s-/" (cmd! (save-excursion (comment-line 1)))
      :n "s-/" #'evilnc-comment-or-uncomment-lines
      :v "s-/" #'evilnc-comment-operator
      :gi  [s-backspace] #'doom/backward-kill-to-bol-and-indent
      :gi  [s-left]      #'doom/backward-to-bol-or-indent
      :gi  [s-right]     #'doom/forward-to-last-non-comment-or-eol
      :gi  [M-backspace] #'backward-kill-word
      :gi  [M-left]      #'backward-word
      :gi  [M-right]     #'forward-word)

;;; leader and localleader keys

;; TODO: try using `set-transient-map' and `doom-lookup-key'
;; TODO: try using `general-key' or `general-simulate-key'
(defun akn/leader (&optional immediate-which-key)
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events (listify-key-sequence (kbd doom-leader-alt-key)))
  (when (and immediate-which-key (fboundp #'which-key--start-timer))
    (which-key--start-timer 0.05)))

(defun akn/localleader ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events (listify-key-sequence (kbd doom-localleader-alt-key)))
  (minibuffer-message "%s- ") doom-localleader-alt-key)
(defun akn/localleader-which-key-immediate ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events (listify-key-sequence (kbd doom-localleader-alt-key)))
  (when (fboundp #'which-key--start-timer)
    (which-key--start-timer 0.001)))
(defconst akn/localleader
  (akn/cmds! (and (fboundp 'which-key--popup-showing-p) (which-key--popup-showing-p))
             #'akn/localleader-which-key-immediate
             #'akn/localleader))
(map!
 :when (modulep! +leader-keys)
 (:leader "m" akn/localleader)
 :nviemorg "M-," akn/localleader
 (:unless (equal doom-leader-alt-key "M-SPC")
   "M-SPC" #'akn/leader)
 (:when (modulep! :editor evil)
   (:map (evil-motion-state-map)
         "," akn/localleader)
   (:map (evil-snipe-override-mode-map evil-snipe-override-local-mode-map)
    :m "," akn/localleader)))

;;; escape

(map!
 ;; Directly run `doom/escape' instead of `evil-normal-state'.
 ;; (For example, then I can use C-g to get out of `multiple-cursors-mode'
 ;; without switching to normal state first.)
 :i "C-g" #'doom/escape
 ;; Use <escape> to escape even in emacs state.
 :ge "<escape>" #'doom/escape)

(when (modulep! :editor evil)
  (add-hook! 'doom-escape-hook :depth 100
    (defun akn/C-g-fallback-evil-normal-state-h ()
      "When I use C-g (which I have mapped directly to `doom/escape'
instead of `evil-normal-state' in insert state), fall back to
escaping to normal state. So C-g can do all the same things as
<escape>, but its priorities are different (escaping from insert
to normal state is deprioritized)."
      (when (memq evil-state '(visual insert emacs))
        (evil-normal-state)
        t))))
(add-hook! 'doom-escape-hook :depth 99
  (defun akn/deactivate-mark-h ()
    (when (region-active-p)
      (deactivate-mark)
      t)))
(after! corfu-popupinfo
  ;; not useful for for escape, but useful for C-g
  (add-hook! 'doom-escape-hook :depth -50
    (defun akn/leave-corfu-h ()
      (when (memq corfu-map (current-minor-mode-maps))
        (corfu-quit)
        t))))
(after! completion-preview
  (add-hook! 'doom-escape-hook :depth -45
    (defun akn/leave-completion-preview-h ()
      (when (memq completion-preview-active-mode-map (current-minor-mode-maps))
        (completion-preview-hide)
        t))))

;;; leader bindings

(map!
 :when (modulep! +leader-bindings)
 :leader

 :desc "Switch project buffer" "," #'consult-project-buffer
 "<" #'switch-to-buffer

 (:prefix "b"
  ;; also SPC b l, but I keep forgetting that
  "o" (if (modulep! :editor evil)
          #'evil-switch-to-windows-last-buffer
        (kmacro "C-x b RET"))
  "L" #'akn/last-buffer-other-window
  (:when (modulep! :ui read-aloud)
   :desc "Toggle talking (read aloud)" "t" #'+read-aloud/toggle)
  (:when (modulep! :ui tab-bar)
   :desc "Switch workspace buffer" "b" #'+workspace/switch-to-buffer
   :desc "Switch buffer"           "B" #'switch-to-buffer
   ;; TODO
   :desc "ibuffer workspace"       "I" #'+ibuffer/open-for-current-workspace))

 (:after flycheck
  :desc "errors" "e" flycheck-command-map)

 (:prefix "f"
  "i" #'dired-jump
  (:prefix ("a" . "fasd")
   :when (modulep! :tools fasd)
   "d" #'+fasd/find-directory-only
   "f" #'+fasd/find-file-only
   "s" #'fasd-find-file
   "a" #'fasd-find-file))

 (:prefix "g"
  (:when (and (modulep! :emacs repeat)
              (modulep! :tools magit))
   "[" #'+repeat/+vc-gutter/previous-hunk
   "]" #'+repeat/+vc-gutter/next-hunk)
  (:when (modulep! :tools stgit)
   "C-s" #'+stgit/status))

 (:prefix "h"
  (:prefix "r"
   "c" #'akn/doom-check-for-updates
   "?" #'akn/doom-check-for-updates
   "R" #'akn/doom-reload-restart
   "u" #'doom/upgrade))

 (:prefix "i"
  (:when (modulep! :editor typing-the-word-blimpy-in-doom-emacs)
   :desc "Blimpy" "B" #'blimpy-type-the-word-blimpy-in-emacs))

 (:prefix "o"
  "C-c" #'akn/open-in-vscode
  "C" #'akn/open-project-in-vscode
  "L" #'akn/open-links
  (:when (modulep! :emacs calc)
   "c" #'calc-dispatch)
  ;; Doom maps this, but then overrides it if (modulep! :emacs dired +dirvish) is enabled.
  (:when (modulep! :ui treemacs)
   :desc "Project sidebar" "p" #'+treemacs/toggle
   :desc "Find file in project sidebar" "P" #'treemacs-find-file))

 (:prefix "p"
  :desc "Switch project" "p" #'akn/switch-project
  :desc "Switch activity" "P" #'activities-resume
  :desc "Find sibling file other window" "O" #'akn/find-sibling-file-other-window
  (:when (modulep! :tools prodigy)
   "S" #'+prodigy/prodigy))

 (:prefix "q"
  "n" #'akn/open-new-emacs
  "N" #'akn/open-new-emacs-with-profile)

 (:prefix "r"
  (:when (and (modulep! :tools regex) (boundp 'akn/pcre2el-keymap))
   :desc "pcre2el" "x"   akn/pcre2el-keymap))

 (:prefix "t"
  (:when (modulep! :ui read-aloud)
   :desc "Toggle talking (read aloud)" "a" #'+read-aloud/toggle)
  (:when (fboundp 'akn/modeline-word-count-mode)
   "W" #'akn/modeline-word-count-mode)
  (:when (fboundp 'akn/indent-guides-mode)
   "i" #'akn/indent-guides-mode
   "C-i" #'akn/global-indent-guides-mode)
  (:when (modulep! :ui tab-bar)
   "C-t" #'+tab-bar-show-mode)
  (:when (modulep! :ui highlight-symbol)
   :desc "Highlight current symbol" "S"   #'symbol-overlay-mode
   :desc "Highlight current symbol" "C-S" #'akn/symbol-overlay-global-mode)
  (:when (modulep! :ui golden-ratio)
   "G" #'golden-ratio-mode)
  (:when (modulep! :ui tabs)
   :desc "Tabs (globally)" "t" #'global-tab-line-mode
   :desc "Tabs (locally)"  "T" #'tab-line-mode)
  (:prefix ("L" . "ligatures")
   :when (modulep! :ui ligatures)
   :desc "Font ligatures"  "f" #'auto-composition-mode
   :desc "Extra ligatures" "e" #'prettify-symbols-mode
   :desc "All ligatures"   "l" #'akn/toggle-all-ligatures)
  (:prefix ("o" . "opacity")
   "o" #'akn/toggle-opacity
   "O" #'akn/set-frame-opacity/alpha-background
   "<up>" #'akn/opacity-up
   "<down>" #'akn/opacity-down
   "k" #'akn/opacity-up
   "j" #'akn/opacity-down))

 (:when (modulep! :lang obsidian)
  ;; Capturing a new note in the inbox
  :desc "Obsidian Capture" "Y" #'akn/obsidian-capture)

 (:when (modulep! :tools fasd)
  "z" #'+fasd/find-directory-only)

 (:when (or (modulep! :ui workspaces)
            (modulep! :ui tab-bar))
  :prefix ("TAB" . "workspace")
  :desc "Display tab bar"           "TAB"     #'+workspace/display
  :desc "Switch workspace"          "."       #'+workspace/switch-to
  :desc "Switch to last workspace"  "`"       #'+workspace/other
  :desc "New workspace"             "n"       #'+workspace/new
  :desc "New named workspace"       "N"       #'+workspace/new-named
  :desc "Load workspace from file"  "l"       #'+workspace/load
  :desc "Save workspace to file"    "s"       #'+workspace/save
  :desc "Kill session"              "x"       #'+workspace/kill-session
  :desc "Kill this workspace"       "d"       #'+workspace/kill
  :desc "Delete saved workspace"    "D"       #'+workspace/delete
  :desc "Rename workspace"          "r"       #'+workspace/rename
  :desc "Restore last session"      "R"       #'+workspace/restore-last-session
  :desc "Next workspace"            "]"       #'+workspace/switch-right
  :desc "Previous workspace"        "["       #'+workspace/switch-left
  :desc "Switch to 1st workspace"   "1"       #'+workspace/switch-to-0
  :desc "Switch to 2nd workspace"   "2"       #'+workspace/switch-to-1
  :desc "Switch to 3rd workspace"   "3"       #'+workspace/switch-to-2
  :desc "Switch to 4th workspace"   "4"       #'+workspace/switch-to-3
  :desc "Switch to 5th workspace"   "5"       #'+workspace/switch-to-4
  :desc "Switch to 6th workspace"   "6"       #'+workspace/switch-to-5
  :desc "Switch to 7th workspace"   "7"       #'+workspace/switch-to-6
  :desc "Switch to 8th workspace"   "8"       #'+workspace/switch-to-7
  :desc "Switch to 9th workspace"   "9"       #'+workspace/switch-to-8
  :desc "Switch to final workspace" "0"       #'+workspace/switch-to-final
  :desc "Swap left"                 "<left>"  #'+workspace/swap-left
  :desc "Swap right"                "<right>" #'+workspace/swap-right
  :desc "Move buffer to workspace"  "m"       #'akn/move-buffer-to-workspace
  :desc "Other workspace prefix"    "O"       #'akn/other-workspace-prefix)
 (:when (modulep! :ui tab-bar)
  :prefix ("TAB" . "workspace")
  :desc "Switch to buffer"          "b" #'+workspace/switch-to-buffer
  :desc "Force keep workspace name" "f" #'+workspace/force-keep-current-name
  :desc "Force auto workspace name" "F" #'+workspace/force-auto-name
  :desc "Kill workspace + buffers"  "k" #'+workspace/kill-buffers-close-workspace
  :desc "Toggle show"               "t" #'+tab-bar/toggle-show
  :desc "Undo close workspace"      "u" #'tab-bar-undo-close-tab
  (:when (modulep! :ui tab-bar -bufferlo)
   :desc "Clear other buffers"       "C" #'tabspaces-clear-buffers
   :desc "Switch to buffer's tab"    "B" #'tabspaces-switch-buffer-and-tab
   :desc "Open project workspace"    "p" #'tabspaces-open-or-create-project-and-workspace)
  (:prefix ("a" . "activities")
   ;; https://github.com/alphapapa/activities.el?tab=readme-ov-file#commands
   :desc "New empty activity"         "n"   #'activities-new
   :desc "Define tab as new activity" "d"   #'activities-define
   :desc "Update default state"       "D"   (cmd! (setq current-prefix-arg '(4)) (call-interactively #'activities-define))
   :desc "Resume activity"            "a"   #'activities-resume
   :desc "Suspend (save+close)"       "s"   #'activities-suspend
   :desc "Close + revert (s+g)"       "k"   #'activities-kill
   :desc "Switch to active activity"  "RET" #'activities-switch
   :desc "Switch buffer"              "b"   #'activities-switch-buffer
   :desc "Revert to default state"    "g"   #'activities-revert
   :desc "List activities"            "l"   #'activities-list))

 ;; not sure about this
 :desc "vlf" "v" (akn/cmds! (bound-and-true-p vlf-mode) vlf-mode-map
                            #'vlf)

 (:prefix "w"
  "D" #'toggle-window-dedicated
  "m h" #'doom/window-maximize-horizontally
  "O" #'other-window-prefix
  "." #'same-window-prefix
  "C-h" nil) ; so this doesn't interfere with using C-h in which-key

 "SPC" (akn/cmds! (when-let* ((p (doom-project-root)))
                    (file-remote-p p))
                  #'consult-project-buffer
                  #'projectile-find-file))

;;; global

(defun +multiple-cursors-use-evil-mc-p ()
  (or (bound-and-true-p evil-mc-mode)
      (not (fboundp 'multiple-cursors-mode))
      (memq (bound-and-true-p evil-state) '(normal visual motion operator replace))))
(defmacro +mc-if-evil-mc (if-evil if-not-evil)
  `(akn/cmds! (+multiple-cursors-use-evil-mc-p) ,if-evil ,if-not-evil))

(map!
 (:when (modulep! :ui highlight-symbol)
  ;; NOTE: `*' (`evil-ex-search-word-forward') and `\#' (`evil-ex-search-word-backward') might be a better way
  :desc "Highlight current symbol occurences" "M-i"  #'symbol-overlay-put)

 (:when (modulep! :editor fold)
   ;; pressing tab in normal mode folds/unfolds
   ;; same as "z a"
   ;; inspired by org-mode
   :m "TAB"   #'+fold/toggle
   :m "<tab>" #'+fold/toggle
   :m "<backtab>" #'+fold/outline-cycle-all-simple
   :m "S-<tab>" #'+fold/outline-cycle-all-simple
   :m "S-TAB" #'+fold/outline-cycle-all-simple
   "M-<up>" #'+fold/drag-stuff-up
   "M-<down>" #'+fold/drag-stuff-down)

 (:when (modulep! :editor multiple-cursors)
   ;; NOTE: most evil-mc bindings are g z _
   ;; https://github.com/gabesoft/evil-mc?tab=readme-ov-file#basic-usage
   ;; There's also evil-multiedit
   :v "A"       #'evil-mc-make-cursor-in-visual-selection-end
   :v "I"       #'evil-mc-make-cursor-in-visual-selection-beg
   "s-d"         (+mc-if-evil-mc #'evil-mc-make-and-goto-next-match                     #'mc/mark-next-like-this-symbol)
   "s-D"         (+mc-if-evil-mc #'evil-mc-make-and-goto-prev-match                     #'mc/mark-previous-like-this-symbol)
   "M-s-<down>"  (+mc-if-evil-mc #'+multiple-cursors/evil-mc-make-cursor-move-next-line #'+multiple-cursors/mc-make-cursor-move-next-line)
   "M-s-<up>"    (+mc-if-evil-mc #'+multiple-cursors/evil-mc-make-cursor-move-prev-line #'+multiple-cursors/mc-make-cursor-move-prev-line)
   "s-<mouse-1>" (+mc-if-evil-mc #'+multiple-cursors/click-add-cursor                   #'mc/toggle-cursor-on-click)
   "M-g z z"     (+mc-if-evil-mc #'+multiple-cursors/evil-mc-toggle-cursor-here         #'+multiple-cursors/mc-toggle-cursor-here)

   "M-g z t"     (+mc-if-evil-mc #'+multiple-cursors/evil-mc-toggle-cursors             #'+multiple-cursors/mc-toggle-cursors)
   "M-s-t"       (+mc-if-evil-mc #'+multiple-cursors/evil-mc-toggle-cursors             #'+multiple-cursors/mc-toggle-cursors)
   "s-y" #'+multiple-cursors/mc-toggle-evil)

 (:when (modulep! :os emacs-mac)
   "H-<up>"    (kmacro "s-<up>")
   "H-<down>"  (kmacro "s-<down>")
   "H-<left>"  (kmacro "s-<left>")
   "H-<right>" (kmacro "s-<right>"))

 "C-s-f" #'akn/fullscreen-toggle

 (:when (modulep! :completion hippie)
   ;; M-/
   [remap dabbrev-expand] #'hippie-expand)

 ;; by default, M-x butterfly C-M-c works, but let's be faithful to the original (C-x M-c M-butterfly)
 (:when (modulep! :app xkcd)
   "C-x M-c M-b" (cmd! (map! :map vertico-map "y" (cmd! (self-insert-command 1)
                                                        (when (equal (buffer-string) "butterfly") (vertico-exit))))
                       (unwind-protect (and (completing-read "" '("butterfly") nil t "b")
                                            (butterfly))
                         (map! :map vertico-map "y" nil))))

 (:when (and (modulep! :tools regex) (boundp 'akn/pcre2el-keymap))
   :desc "pcre2el" "C-c /" akn/pcre2el-keymap)

 (:when (or (modulep! :ui workspaces)
            (modulep! :ui tab-bar))
   ;;command-option-left and -right move between workspaces
   "M-s-<left>"  #'+workspace/switch-left
   "M-s-<right>" #'+workspace/switch-right
   "M-s-w"       #'+workspace/kill

   "s-t"   #'+workspace/new
   "s-T"   #'+workspace/display

   ;; This is built in, but not for insert mode for some reason.
   :gn "s-1" #'+workspace/switch-to-0
   :gn "s-2" #'+workspace/switch-to-1
   :gn "s-3" #'+workspace/switch-to-2
   :gn "s-4" #'+workspace/switch-to-3
   :gn "s-5" #'+workspace/switch-to-4
   :gn "s-6" #'+workspace/switch-to-5
   :gn "s-7" #'+workspace/switch-to-6
   :gn "s-8" #'+workspace/switch-to-7
   :gn "s-9" #'+workspace/switch-to-final
   (:leader "TAB 9" #'+workspace/switch-to-final)

   "C-s-<left>" #'+workspace/switch-left
   "C-s-<right>" #'+workspace/switch-right
   "s-{" #'+workspace/switch-left
   "s-}" #'+workspace/switch-right)
 (:after tab-bar
  (:map tab-bar-history-mode-map
   [remap winner-undo] #'tab-bar-history-back
   [remap winner-redo] #'tab-bar-history-forward
   ;; inspired by browser shortcuts
   "s-T" #'tab-bar-undo-close-tab)
  (:map tab-bar-mode-map
   "s-B" #'+tab-bar-show-mode)
  (:map tab-bar-map
   ;; interferes with context menu
   "<wheel-up>"      #'ignore
   "<wheel-down>"    #'ignore))

 (:when (modulep! :ui tabs)
   "s-[" (akn/cmds! (and (+popup-window-p) (bound-and-true-p tab-line-mode))
                    #'tab-line-switch-to-prev-tab
                    #'previous-buffer)
   "s-]" (akn/cmds! (and (+popup-window-p) (bound-and-true-p tab-line-mode))
                    #'tab-line-switch-to-next-tab
                    #'next-buffer))

 "s-w" (akn/cmds! (or (and buffer-file-name (not (buffer-modified-p)))
                      (derived-mode-p 'Custom-mode))
                  #'bury-buffer
                  (fboundp 'boring-processes-kill-current-buffer)
                  #'boring-processes-kill-current-buffer
                  #'kill-current-buffer)

 "C-M-<return>" #'toggle-frame-maximized

 "s-W" #'delete-window
 "s-`" #'other-window
 "M-o" #'other-window

 "M-s-s" #'projectile-save-project-buffers

 "s-h" #'windmove-left
 "s-j" #'windmove-down
 "s-k" #'windmove-up
 "s-l" #'windmove-right
 "s-H" #'fm-left-frame
 "s-J" #'fm-down-frame
 "s-K" #'fm-up-frame
 "s-L" #'fm-right-frame
 (:when (modulep! :ui buffer-move)
   "C-s-h" #'buf-move-left
   "C-s-l" #'buf-move-right
   "C-s-k" #'buf-move-up
   "C-s-j" #'buf-move-down
   "C-s-<left>" #'buf-move-left
   "C-s-<right>" #'buf-move-right
   "C-s-<up>" #'buf-move-up
   "C-s-<down>" #'buf-move-down)

 ;; Firefox reader mode shortcut
 "M-s-r" #'view-mode

 "M-s-h" #'evil-window-vsplit
 "M-s-j" #'+evil/window-split-and-follow
 "M-s-k" #'evil-window-split
 "M-s-l" #'+evil/window-vsplit-and-follow
 (:when (not (modulep! :editor evil))
   "M-s-h" #'split-window-right
   "M-s-j" (cmd! (select-window (split-window-right)))
   "M-s-k" #'split-window-below
   "M-s-l" (cmd! (select-window (split-window-below))))
 "M-s-H" #'split-root-window-right
 "M-s-L" (cmd! (select-window (split-root-window-right)))
 "M-s-K" #'split-root-window-below
 "M-s-J" (cmd! (select-window (split-root-window-below)))

 (:when (modulep! :editor words)
  "H-w" #'+words/toggle-subword-superword)

 ;; https://old.reddit.com/r/neovim/comments/1cvur6s/what_custom_text_objects_do_you_use/
 ;; https://github.com/chrisgrieser/nvim-various-textobjs
 :o "n" #'akn/evil-almost-end-of-line

 "C-S-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "C-S-x" (akn/cmds! (region-active-p) #'kill-region
                    #'kill-whole-line)
 "C-S-v" #'yank

 ;; by default, Doom only maps this to cut in visual state.
 ;; so it doesn't work while highlighting something in insert state.
 "s-x" (akn/cmds! (region-active-p) #'kill-region
                  #'kill-whole-line)

 "s-V" #'akn/yank-no-transform
 "M-s-v" #'akn/yank-reindent

 ;;command-up and command-down
 "s-<up>" #'beginning-of-buffer
 "s-<down>" #'end-of-buffer

 ;;like vscode
 "s-p" #'projectile-find-file
 ;; NOTE: could also use +default/search-project (SPACE s p)
 "s-F" #'+default/search-project
 "s-P" #'execute-extended-command
 "M-s-s" #'projectile-save-project-buffers

 "s-o" #'akn/system-find-file

 "s-?" #'which-key-show-major-mode

 ;; C-u is taken by evil, so we use M-u
 "M-u" #'universal-argument
 "s-u" #'universal-argument
 (:map universal-argument-map
       "M-u" #'universal-argument-more
       "s-u" #'universal-argument-more
       "C-u" nil)
 ;; can use either C-1 or M-1
 "M-1" #'digit-argument
 "M-2" #'digit-argument
 "M-3" #'digit-argument
 "M-4" #'digit-argument
 "M-5" #'digit-argument
 "M-6" #'digit-argument
 "M-7" #'digit-argument
 "M-9" #'digit-argument
 "M-0" #'digit-argument

 "C-l" #'recenter-top-bottom

 ;; same as my vscode keybindings
 :gnm "C--" #'better-jumper-jump-backward
 :gnm "C-=" #'better-jumper-jump-forward
 :gnm "C-_" #'better-jumper-jump-forward
 :gnm "C-+" nil
 ;; since we overrode this in normal mode
 "M-s--" #'text-scale-adjust ; #'text-scale-decrease
 "M-s-=" #'text-scale-adjust ; #'text-scale-increase
 "M-s-0" #'text-scale-adjust ; (cmd! (text-scale-mode -1))

 ;; like vscode
 "S-M-<up>" #'akn/duplicate-up
 "S-M-<down>" #'akn/duplicate-down

 ;; By default, it's `count-words-region', which always uses the region, even if it's inactive.
 ;; `count-words' uses the whole buffer if the region is inactive.
 ;; And org-word-count ignores link locations.
 "M-=" (akn/cmds! (derived-mode-p 'org-mode) #'org-word-count
                  #'count-words) ;this is basically dwim

 "M-S-SPC" #'cycle-spacing

 ;; from https://git.sr.ht/~technomancy/better-defaults/tree/main/item/better-defaults.el
 [remap zap-to-char] #'zap-up-to-char                ;M-z
 [remap isearch-forward] #'isearch-forward-regexp    ;C-s
 [remap isearch-backward] #'isearch-backward-regexp  ;C-r
 [remap isearch-forward-regexp] #'isearch-forward    ;C-M-s
 [remap isearch-backward-regexp] #'isearch-backward  ;C-M-r

 ;; inspired by https://github.com/bbatsov/crux
 [remap capitalize-word]   (akn/cmds! transient-mark-mode #'capitalize-dwim #'capitalize-word) ;M-c
 [remap capitalize-region] (akn/cmds! transient-mark-mode #'capitalize-dwim #'capitalize-region)
 [remap upcase-word]       (akn/cmds! transient-mark-mode #'upcase-dwim     #'upcase-word)
 [remap upcase-region]     (akn/cmds! transient-mark-mode #'upcase-dwim     #'upcase-region) ;C-x C-u
 [remap downcase-word]     (akn/cmds! transient-mark-mode #'downcase-dwim   #'downcase-word) ;M-l
 [remap downcase-region]   (akn/cmds! transient-mark-mode #'downcase-dwim   #'downcase-region) ;C-x C-l

 ;; `prog-fill-reindent-defun' is nice, but when the region is active, I always
 ;; want it to fill-paragraph, not reindent.
 [remap prog-fill-reindent-defun] (akn/cmds! (region-active-p) #'fill-paragraph
                                             #'prog-fill-reindent-defun)

 ;; shift click
 ;; https://superuser.com/questions/521223/shift-click-to-extend-marked-region
 "S-<down-mouse-1>" #'mouse-save-then-kill
 "S-<mouse-1>" #'ignore-preserving-kill-region
 ;; shift left and shift right
 :i "S-<left>"  nil
 :i "S-<right>" nil

 ;; command-backslash
 ;; like DrRacket (except it inserts the word lambda, not a unicode lambda)
 "s-\\" (cmd! (insert "lambda"))

 ;; https://www.emacswiki.org/emacs/Scrolling#h5o-5
 "C-<mouse-4>" (kbd "<up>")
 "C-<mouse-5>" (kbd "<down>")
 "C-<triple-wheel-up>" (kbd "<up>")
 "C-<triple-wheel-down>" (kbd "<down>")

 ;; can also see these in `key-translation-map'
 "C-x 8 C-h" (cmd! (describe-bindings (kbd "C-x 8")) (akn/after-timer! (0) (outline-show-all)))

 :m "RET" (akn/cmds! (command-remapping #'evil-ret) it
                     (and (get-char-property (point) 'button)
                          (get-char-property (point) 'category)
                          (require 'button nil t)
                          (button-at (point)))
                     #'push-button
                     (and (or (get-char-property (point) 'field)
                              (get-char-property (point) 'button)
                              (get-char-property (point) 'widget-doc))
                          (require 'wid-edit nil t)
                          (widget-at (point)))
                     #'widget-button-press
                     (thing-at-point-url-at-point) #'browse-url-at-point
                     (thing-at-point-file-at-point) #'find-file-at-point
                     (derived-mode-p 'helpful-mode) #'helpful-at-point
                     (derived-mode-p 'embark-collect-mode) #'embark-act
                     ;; (doom-thing-at-point-or-region) #'+lookup/definition
                     (let ((arg current-prefix-arg))
                       (require 'embark)
                       (if-let* ((targets (embark--targets)))
                             (let* ((target
                                     (or (nth
                                          (if (or (null arg) (minibufferp))
                                              0
                                            (mod (prefix-numeric-value arg) (length targets)))
                                          targets)))
                                    (type (plist-get target :type))
                                    (default-action (embark--default-action type))
                                    (action (or (command-remapping default-action) default-action)))
                               action)))
                     #'embark-dwim
                     ;; t #'embark dwim
                     ;; #'evil-ret
                     (cmd! (message "nothing to do")))

 "C-'" #'imenu

 ;; search
 (:when (modulep! :completion vertico)
   "C-S-r"        #'vertico-repeat)

 ;; buffer management
 (:when (modulep! :ui workspaces)
   "C-x b"       #'persp-switch-to-buffer
   "C-x B"       #'switch-to-buffer
   "C-x 4 B"     #'switch-to-buffer-other-window
   (:when (modulep! :completion ivy)
     "C-x 4 b"   #'+ivy/switch-workspace-buffer-other-window))
 (:when (modulep! :ui tab-bar)
   "C-x b"       #'tabspaces-switch-to-buffer
   "C-x B"       #'switch-to-buffer
   "C-x 4 B"     #'switch-to-buffer-other-window)
 "C-x C-b"     #'ibuffer ; instead of `list-buffers' (also see `bs-show', `electric-buffer-list')
 "C-x K"       #'doom/kill-this-buffer-in-all-windows

 (:when (fboundp 'er/expand-region)
   ;; expand-region
   ;; https://micro.rousette.org.uk/2021/01/03/a-useful-binding.html
   (:map 'override
    :v "v" (akn/cmds! (not (memq (evil-visual-type) '(block line)))
                      #'er/expand-region)))

 (:after flycheck
  :map flycheck-command-map
  "N" #'flycheck-previous-error)

 ;; smartparens
 (:after smartparens
  :map smartparens-mode-map
  "C-M-a"           #'sp-beginning-of-sexp
  "C-M-e"           #'sp-end-of-sexp
  "C-M-f"           #'sp-forward-sexp
  "C-M-b"           #'sp-backward-sexp
  "C-M-n"           #'sp-next-sexp
  "C-M-p"           #'sp-previous-sexp
  "C-M-u"           #'sp-up-sexp
  "C-M-d"           #'sp-down-sexp
  "C-M-k"           #'sp-kill-sexp
  "C-M-t"           #'sp-transpose-sexp
  "C-M-<backspace>" #'sp-splice-sexp

  ;;my extras, https://gist.github.com/jsmestad/1059d15e8debf5f2e7e81c92052c67d2
  "M-(" #'sp-wrap-round
  "C-(" #'sp-wrap-round
  "M-[" (akn/cmds! (display-graphic-p) #'sp-wrap-square)
  ;; ^caused weird problems in terminal emacs.
  ;;  When I move the mouse around in iTerm emacs, it interprets that as M- open bracket for some reason
  "C-{" #'sp-wrap-curly            ;not M-{ because that's bound to `backward-paragraph'
  "C-M-h" #'sp-backward-slurp-sexp "C-S-h" #'sp-backward-slurp-sexp
  "C-M-l" #'sp-backward-barf-sexp  "C-S-l" #'sp-backward-barf-sexp
  "M-L"   #'sp-forward-slurp-sexp
  "M-H"   #'sp-forward-barf-sexp
  "C-M-[" #'sp-select-previous-thing
  "C-M-]" #'sp-select-next-thing

  ;; :i "C-." (kbd! ">" :state 'normal)
  ;; :i "C-," (kbd! "<" :state 'normal)
  ;; :i "C->" (kbd! ">" :state 'normal)
  ;; :i "C-<" (kbd! "<" :state 'normal)
  ;; https://github.com/MaxSt/dotfiles/blob/master/emacs.d/config.org#smartparens
  ;; https://github.com/tpope/vim-sexp-mappings-for-regular-people
  :n "(" (akn/cmds! (apply #'derived-mode-p akn/lisp-like-modes)
                    #'sp-backward-up-sexp)
  :n ")" (akn/cmds! (apply #'derived-mode-p akn/lisp-like-modes)
                    (cmd!
                     (forward-char)
                     (sp-backward-up-sexp)
                     (evil-jump-item)))
  :n ">" (general-key-dispatch #'evil-shift-right
           "I" (cmd!
                 (sp-end-of-sexp)
                 (when (not (char-equal ?\) (preceding-char)))
                   (insert " "))
                 (evil-insert 1))
           ")" #'sp-forward-slurp-sexp
           "(" #'sp-backward-barf-sexp)
  :n "<" (general-key-dispatch #'evil-shift-left
           "I" (cmd!
                 (sp-beginning-of-sexp)
                 (when (not (char-equal ?\) (following-char)))
                   (insert " ")
                   (evil-backward-char))
                 (evil-insert 1))
           ")" #'sp-forward-barf-sexp
           "(" #'sp-backward-slurp-sexp)))

;;; major/minor mode keymaps

(map!
 (:when (modulep! :editor fold)
   :after cus-edit
   :map custom-mode-map
   "TAB" nil
   "<tab>" nil
   :n "TAB" nil
   :n "<tab>" nil
   "<backtab>" nil
   "S-<tab>" nil
   :n "<backtab>" nil
   :n "S-<tab>" nil)

 (:when (modulep! :emacs calc)
   (:after calc
    :map calc-mode-map
    (:when (fboundp 'casual-calc-tmenu)
      "C-o"         #'casual-calc-tmenu)
    "s-?"         #'calc-transient
    "s-z"         #'calc-undo
    "s-Z"         #'calc-redo
    "s-v"         #'calc-yank
    "s-c"         (akn/cmds! (not (use-region-p)) #'calc-copy-as-kill)
    "s-x"         #'calc-kill
    :e "<escape>" #'doom/escape
    :e "SPC"      #'doom/leader
    "C-c C-k" #'calc-quit)

   (:after calc-yank
    :map calc-edit-mode-map
    "C-c C-k" #'calc-quit))

 (:when (modulep! :tools regex)
   :map emacs-lisp-mode-map
   (:localleader "x" #'+regex/xr-at-point)
   (:localleader "X" (akn/cmds! (and (not (nth 3 (syntax-ppss))) (memq (char-after) '(?\( ?\))) (fboundp 'rxt-toggle-elisp-rx))
                                #'rxt-toggle-elisp-rx
                                #'+regex/xr-try-replace-at-point)))

 (:map help-map
            "s-;" (cmd! (message "%s"
                                 (let ((arg current-prefix-arg))
                                  (if-let* ((targets (embark--targets)))
                                        (let* ((target
                                                (or (nth
                                                     (if (or (null arg) (minibufferp))
                                                         0
                                                       (mod (prefix-numeric-value arg) (length targets)))
                                                     targets)))
                                               (type (plist-get target :type))
                                               (default-action (embark--default-action type))
                                               (action (or (command-remapping default-action) default-action)))
                                          action)
                                    "no command")))))

 (:when (and (modulep! :editor evil) (modulep! +evil-insert))
   :gi [s-backspace] #'evil-delete-back-to-indentation)

 (:when (modulep! :ui doom-dashboard)
   "s-<return>" #'akn/fullscreen-toggle)

 (:after evil-snipe
  :map evil-snipe-local-mode-map
  :nm "s" #'evil-avy-goto-char-timer)

 (:after tex-mode
  :map latex-mode-map
  :localleader "v" #'latex-preview-pane-mode)

 (:map (text-mode-map org-mode-map markdown-mode-map akn/new-file-mode-map global-map)
       "s-C" #'count-words)

      ;; with-editor-mode is used by commit message editors in magit, for example
 (:after with-editor
  :map with-editor-mode-map
  ;; like vscode commit messages
  :nvieomrg "s-<return>" #'with-editor-finish))

;;; expand-region
;; This was removed from doom for evil users
(use-package expand-region
  :defer-incrementally t
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  (defadvice! doom--quit-expand-region-a (&rest _)
    "Properly abort an expand-region region."
    :before '(evil-escape doom/escape evil-exit-visual-state)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0))))

;;; delete-selection-mode
(delete-selection-mode)

;;; akn/active-region-arrow-boundary-mode

(define-minor-mode akn/active-region-arrow-boundary-mode
  "Change behavior of non-shifted arrow keys when region is active.

When region is active, <right> goes to end of region and <left> goes to
beginning of region."
  :keymap (make-sparse-keymap)
  :group 'akn)
(map! :map akn/active-region-arrow-boundary-mode-map
      "<left>"  (akn/cmds! (and akn/active-region-arrow-boundary-mode
                                (use-region-p)
                                (eq shift-select-mode t)
                                (not (bound-and-true-p rectangle-mark-mode))
                                (eq (car-safe transient-mark-mode) 'only) ;we're currently in a shift-selection (as opposed to having called `set-mark-command' directly, for example)
                                (or (not current-prefix-arg) (eq (prefix-numeric-value current-prefix-arg) 1)))
                           (akn/defun akn/goto-region-beginning (&optional n)
                             (interactive "^p")
                             (if (or this-command-keys-shift-translated (not (or (not n) (eq n 1))))
                                 (let ((akn/active-region-arrow-boundary-mode nil))
                                   ;; https://emacs.stackexchange.com/a/59509
                                   (call-interactively (key-binding (this-command-keys))))
                               (goto-char (region-beginning))
                               (deactivate-mark))))
      "<right>" (akn/cmds! (and akn/active-region-arrow-boundary-mode
                                (use-region-p)
                                (eq shift-select-mode t)
                                (not (bound-and-true-p rectangle-mark-mode))
                                (eq (car-safe transient-mark-mode) 'only) ;we're currently in a shift-selection (as opposed to having called `set-mark-command' directly, for example)
                                (or (not current-prefix-arg) (eq (prefix-numeric-value current-prefix-arg) 1)))
                           (akn/defun akn/goto-region-end (&optional n)
                             (interactive "^p")
                             (if (or this-command-keys-shift-translated (not (or (not n) (eq n 1))))
                                 (let ((akn/active-region-arrow-boundary-mode nil))
                                   ;; https://emacs.stackexchange.com/a/59509
                                   (call-interactively (key-binding (this-command-keys))))
                               (goto-char (region-end))
                               (deactivate-mark)))))

(define-globalized-minor-mode akn/active-region-arrow-boundary-global-mode
  akn/active-region-arrow-boundary-mode akn/active-region-arrow-boundary-mode
  :predicate t
  :group 'akn)
(akn/active-region-arrow-boundary-global-mode)

;;; s-e
;; see manual section "Keymaps for Translating Sequences of Events"

;; working together with iTerm mapping
;; We're making Emacs interpret S-<f12> to mean command.
;; e.g. iTerm maps "s-j" to "S-<f12> j" ("^[[24;2~j"), then Emacs maps "S-<f12> j" to "s-j".

;; Also, I'm making iTerm map "s-e" in particular to just "S-<f12>" ("^[[24;2~"),
;; so I can type "s-e c" in iTerm to get "s-c", for example

(keymap-set key-translation-map "S-<f12>" #'event-apply-super-modifier)
(keymap-set key-translation-map "s-e" #'event-apply-super-modifier) ;for consistency with iTerm

;;; evil insert
(when (modulep! +evil-insert)
  (setq! evil-disable-insert-state-bindings t))

;;;; evil insert state C-x

;;           # EMACS DEFAULT      # EVIL DEFAULT IN INSERT STATE # DOOM DEFAULT (unless `evil-disable-insert-state-bindings')
;; "C-x C-n" #'set-goal-column    #'evil-complete-next-line      #'cape-dabbrev               # Complete next symbol at point
;; "C-x C-p" #'mark-page          #'evil-complete-previous-line  #'+corfu/dabbrev-this-buffer # Complete previous symbol at point
;; "C-x C-l" #'downcase-region    #                              #'cape-line                  # Complete full line
;; "C-x C-k" # kmacro-keymap      #                              #'cape-keyword               # Complete from dictionary/keyword
;; "C-x C-f" #'find-file          #                              #'cape-file                  # Complete file path
;; "C-x C-]" #<undefined>         #                              #'complete-tag               # Complete etags
;; "C-x s"   #'save-some-buffers  #                              #'cape-dict                  # Complete spelling suggestions
;; "C-x C-s" #'save-buffer        #                              #'yasnippet-capf             # Complete snippet
;; "C-x C-o" #'delete-blank-lines #                              #'completion-at-point        # Invoke complete-at-point function

(defvar-keymap akn/vim-omni-completion-map
  "C-n" (cons "Complete next symbol at point"     #'cape-dabbrev)
  "C-p" (cons "Complete previous symbol at point" #'+corfu/dabbrev-this-buffer)
  "C-l" (cons "Complete full line"                #'cape-line)
  "C-k" (cons "Complete from dictionary/keyword"  #'cape-keyword)
  "C-f" (cons "Complete file path"                #'cape-file)
  "C-]" (cons "Complete etags"                    #'complete-tag)
  "s"   (cons "Complete spelling suggestions"     #'cape-dict)
  "C-s" (cons "Complete snippet"                  #'yasnippet-capf)
  "C-o" (cons "Invoke complete-at-point function" #'completion-at-point))

(map! :when (and (modulep! :editor evil) (modulep! +evil-insert))
      :desc "vim-omni-complete" :i "C-S-x" akn/vim-omni-completion-map
      "C-x C-S-n" (defun akn/unset-goal-column ()
                    (interactive)
                    (set-goal-column 'clear)))

;;;; evil insert bindings

;; (see `evil-insert-state-bindings', `evil-update-insert-state-bindings', `evil-disable-insert-state-bindings')
;;           # EMACS DEFAULT           # EVIL DEFAULT IN INSERT STATE              # DOOM DEFAULT (unless `evil-disable-insert-state-bindings')
;; "C-q"     #'quoted-insert           #'evil-quoted-insert
;; "C-v"     #'scroll-up-command       #'evil-quoted-insert
;; "C-k"     #'kill-line               #'evil-insert-digraph
;; "C-o"     #'open-line               #'evil-execute-in-normal-state
;; "C-r"     #'isearch-backward-regexp #'evil-paste-from-register
;; "C-y"     #'yank                    #'evil-copy-from-above
;; "C-e"     #'move-end-of-line        #'evil-copy-from-below
;; "C-n"     #'next-line               #'evil-complete-next                        #'+corfu/dabbrev-or-next (corfu-mode-map)
;; "C-p"     #'previous-line           #'evil-complete-previous                    #'+corfu/dabbrev-or-last (corfu-mode-map)
;; "C-t"     #'transpose-chars         #'evil-shift-right-line
;; "C-d"     #'delete-char             #'evil-shift-left-line
;; "C-w"     #'kill-region             #(if   evil-want-C-w-delete 'evil-delete-backward-word  evil-window-map)
;; "C-u"     #'universal-argument      #(when evil-want-C-u-delete 'evil-delete-back-to-indentation)
;; "C-h"     # help-map                #(when evil-want-C-h-delete 'evil-delete-backward-char-and-join)
;; "C-a"     #'move-beginning-of-line  #'evil-paste-last-insertion
;; "C-@"     #'set-mark-command        #'evil-paste-last-insertion-and-stop-insert #'completion-at-point (corfu-mode-map)
;; "C-SPC"   #'set-mark-command        #                                           #'completion-at-point (corfu-mode-map) / #'corfu-insert-separator (corfu-map)

(map!
 :when (and (modulep! :editor evil) (modulep! +evil-insert))

 :i [remap quoted-insert] #'evil-quoted-insert
 :i "C-u" (when evil-want-C-u-delete #'evil-delete-back-to-indentation)

 :i "C-o" #'evil-execute-in-normal-state
 :i "C-r" #'evil-paste-from-register
 :i "C-t" #'evil-shift-right-line
 :i "C-S-t" #'evil-shift-left-line      ; my alternative for C-d
 :i "C-d" #'evil-shift-left-line

 ;; :i "C-S-v" #'evil-quoted-insert ; using for paste (for Linux/Windows consistency with terminal); use C-q instead
 :i "C-S-k" #'evil-insert-digraph
 :i "C-S-o" #'open-line
 ;; :i "C-S-r" #'isearch-backward-regexp
 :i "C-S-y" #'evil-copy-from-above
 :i "C-S-e" #'evil-copy-from-below
 :i "C-S-n" #'evil-complete-next
 :i "C-S-p" #'evil-complete-previous
 ;; :i "C-S-t" #'transpose-chars
 :i "C-S-d" #'delete-char
 :i "C-S-w" evil-window-map
 :i "C-S-a" #'evil-paste-last-insertion)

;;; evil other bindings

;;            # EMACS DEFAULT           # EVIL DEFAULT                       # DOOM DEFAULT
;; :n "C-n"   #'next-line               #'evil-paste-pop-next
;; :n "C-p"   #'previous-line           #'evil-paste-pop
;; :n "C-t"   #'transpose-chars         #'pop-tag-mark                       #'+workspace/new
;; :n "C-."   #<undefined>              #'evil-repeat-pop
;; :n "C-r"   #'isearch-backward-regexp #'evil-redo
;; :n "C-SPC" #'set-mark-command        #                                    #(cmd! (call-interactively 'evil-insert-state) (call-interactively 'completion-at-point))
;; :v "C-SPC" #'set-mark-command        #                                    #(cmd! (call-interactively 'evil-change)       (call-interactively 'completion-at-point))
;; :n "C--"   #'negative-argument       #                                    #'text-scale-decrease
;; :m "C-6"   #'digit-argument          #'evil-switch-to-windows-last-buffer #
;; :m "C-^"   #<undefined>              #'evil-buffer
;; :m "C-]"   #'abort-recursive-edit    #'evil-jump-to-tag
;; :m "C-b"   #'backward-char           #'evil-scroll-page-up
;; :m "C-e"   #'move-end-of-line        #'evil-scroll-line-down
;; :m "C-f"   #'forward-char            #'evil-scroll-page-down
;; :m "C-o"   #'open-line               #'evil-jump-backward
;; :m "C-y"   #'yank (paste)            #'evil-scroll-line-up
;; :m "C-v"   #'scroll-up-command       #'evil-visual-block
;; :m "C-i"   #'indent-for-tab-command  #'evil-jump-forward
;; :m "C-u"   #'universal-argument      #'evil-scroll-up
;; :m "C-d"   #'delete-char             #'evil-scroll-down
;; :r "C-h"   # help-map                #'evil-replace-backspace
;; :g "C-x p" # project-prefix-map      #                                    #'+popup/other (if non-evil)

(map!
 :n "C-n" (akn/cmds! (memq last-command '(yank evil-paste-before evil-paste-after evil-paste-pop))
                     #'evil-paste-pop-next)
 :n "C-p" (akn/cmds! (memq last-command '(yank evil-paste-before evil-paste-after evil-paste-pop))
                     #'evil-paste-pop)
 :n "C-t" nil
 :nv "C-SPC" nil
 :nv [remap completion-at-point] (akn/defun akn/completion-at-point ()
                                   (interactive)
                                   (call-interactively (if (evil-visual-state-p) #'evil-change #'evil-insert-state))
                                   (call-interactively #'completion-at-point)))
