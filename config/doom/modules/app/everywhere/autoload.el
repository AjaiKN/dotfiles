;;; app/everywhere/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun akn/focus-firefox ()
  ;; https://apple.stackexchange.com/a/276462
  (shut-up (start-process-shell-command "focus-this-frame" nil "osascript -e 'tell application \"System Events\" to tell process \"Firefox\"' -e 'set frontmost to true' -e 'if windows is not {} then perform action \"AXRaise\" of item 1 of windows' -e 'end tell'")))
