;;; app/everywhere/autoload.el -*- lexical-binding: t; -*-

(require 'akn)

;;;###autoload
(defun +everywhere--focus-firefox-h ()
  ;; https://apple.stackexchange.com/a/276462
  (shut-up (start-process-shell-command "focus-this-frame" nil "osascript -e 'tell application \"System Events\" to tell process \"Firefox\"' -e 'set frontmost to true' -e 'if windows is not {} then perform action \"AXRaise\" of item 1 of windows' -e 'end tell'")))

;;;###autoload
(defun +everywhere--frame-setup-h ()
  (set-frame-parameter (selected-frame) 'tab-bar-lines 0)
  (set-frame-parameter (selected-frame) 'tab-bar-lines-keep-state t)
  (akn/doom/set-frame-opacity 90))

;;;###autoload
(defun +everywhere--buffer-setup-h ()
  (tab-line-mode -1))
