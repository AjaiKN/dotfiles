;;; app/transcribe/config.el -*- lexical-binding: t; -*-

(define-minor-mode +transcribe-mode
  ""
  :group 'akn
  :keymap (make-sparse-keymap)
  (setq-local emms-seek-seconds 3))
(map! :map +transcribe-mode-map
      "C-x C-p"   #'emms-play-file
      :gimne "8"  #'emms-seek-backward
      :gimne "9"  #'emms-pause
      :gimne "0"  #'emms-seek-forward
      :gimne "["  (akn/cmds! current-prefix-arg (cmd! (insert-char ?\[)) #'emms-seek-backward)
      :gimne "]"  #'ignore ; #'emms-seek-forward
      :gimne "\\" #'emms-pause
      :i "{" (cmd! (insert-char ?\[))
      :i "}" (cmd! (insert-char ?\])))
(when (modulep! :editor evil)
  (evil-make-intercept-map +transcribe-mode-map '(insert normal motion emacs)))

(use-package! emms
  :defer t
  :commands (emms-play-file emms-seek-backward emms-seek-forward emms-pause)
  :config
  ;; from https://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/transcribe.el
  (emms-all)
  (emms-default-players)
  (if t (require 'emms-player-mpg321-remote))
  (defvar emms-player-list)
  (push 'emms-player-mpg321-remote emms-player-list)
  (require 'emms-mode-line)
  (emms-mode-line-mode 1)
  (require 'emms-playing-time)
  (emms-playing-time-mode 1))
