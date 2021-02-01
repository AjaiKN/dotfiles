;;; evil-quickscope.el --- Highlight unique characters in words for f,F,t,T navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Michael Chen

;; Author: Michael Chen <blorbx@gmail.com>
;; Maintainer: Michael Chen <blorbx@gmail.com>
;; Created: 12 Aug 2015
;; Version: 0.1.4

;; Homepage: http://github.com/blorbx/evil-quickscope
;; Keywords: faces, emulation, vim, evil
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package emulates quick_scope.vim by Brian Le
;; (https://github.com/unblevable/quick-scope). It highlights targets for
;; evil-mode's f,F,t,T keys, allowing for quick navigation within a line with no
;; additional mappings.
;;
;; The functionality is wrapped into two different minor modes. Only one can be
;; activated at a time.

;; evil-quickscope-always-mode provides targets at all times and directly
;; emulates quick_scope.vim. It can be activated by adding the following to
;; ~/.emacs:
;;
;;     (require 'evil-quickscope)
;;     (global-evil-quickscope-always-mode 1)
;;
;; Alternatively, you can enable evil-quickscope-always-mode in certain modes by
;; adding 'evil-quickscope-always-mode-turn-on' to the mode hook. For example:
;;
;;     (add-hook 'prog-mode-hook 'evil-quickscope-always-mode-turn-on)
;;
;; evil-quickscope-mode provides targets only after one of the f,F,t,T keys are
;; pressed. It can be activated by adding the following to ~/.emacs:
;;
;;     (require 'evil-quickscope)
;;     (global-evil-quickscope-mode 1)
;;
;; Or, you can use 'evil-quickscope-mode-turn-on' as a mode hook:
;;
;;     (add-hook 'prog-mode-hook 'evil-quickscope-mode-turn-on)
;;
;; This program requires EVIL (http://bitbucket.org/lyro/evil/wiki/Home)

;;; Code:

(require 'evil)

(defgroup evil-quickscope nil
  "Target highlighting for evil-mode's f,F,t,T keys."
  :group 'evil)

;;; Faces
(defface evil-quickscope-first-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for first unique character."
  :group 'evil-quickscope)

(defface evil-quickscope-second-face
  '((t (:inherit font-lock-keyword-face :underline t)))
  "Face for second unique character."
  :group 'evil-quickscope)

;;; Settings
(defcustom evil-quickscope-bidirectional nil
  "Determines whether overlay only shows in direction of F/T (nil)
or both directions (t)."
  :group 'evil-quickscope
  :type '(boolean))

(defcustom evil-quickscope-cross-lines nil
  "Whether to cross lines for targets.
Use in conjunction with the evil-cross-lines variable."
  :group 'evil-quickscope
  :type '(boolean))

(defcustom evil-quickscope-disable-in-comments nil
  "If enabled (t), disables quickscope-always-mode overlays when in a comment."
  :group 'evil-quickscope
  :type '(boolean))

(defcustom evil-quickscope-search-max 1000
  "Specifies maximum number of characters to search. nil to disable."
  :group 'evil-quickscope
  :type '(natnum))

(defcustom evil-quickscope-always-mode-delay 0.1
  "Seconds to wait before displaying overlays in always-mode.
Usually should be longer than the keyboard repeat rate to prevent excessive
updating when holding a key to scroll. Set to 0 to disable."
  :group 'evil-quickscope
  :type '(number))

;;; Internal variables
(defvar evil-quickscope-always-mode-timer nil
  "Timer for delaying always-mode.")
(make-variable-buffer-local 'evil-quickscope-always-mode-timer)

(defvar evil-quickscope-mode-map
  (let ((evil-quickscope--map (make-sparse-keymap)))
    (evil-define-key 'motion evil-quickscope--map "f" 'evil-quickscope-find-char)
    (evil-define-key 'motion evil-quickscope--map "F" 'evil-quickscope-find-char-backward)
    (evil-define-key 'motion evil-quickscope--map "t" 'evil-quickscope-find-char-to)
    (evil-define-key 'motion evil-quickscope--map "T" 'evil-quickscope-find-char-to-backward)
    evil-quickscope--map)
  "Keymap for `evil-quickscope-mode'.")

;;; Utility functions
(defun evil-quickscope-increment-plist-char (char-plist char)
  "Add count to corresponding char in plist."
  (plist-put char-plist char
             (1+ (or (plist-get char-plist char) 0))))

;;; Character Finding Functions
(defvar evil-quickscope--hl-chars)
(defvar evil-quickscope--word-hl-chars)
(defvar evil-quickscope--seen-chars)
(defvar evil-quickscope--char)

(defun evil-quickscope--update-hl-chars (pos)
  "Checks if char at POS is separator/invalid, if not update
evil-quickscope--seen-chars list."
  (let ((evil-quickscope--char (char-after pos)))
    (evil-quickscope--add-to-hl-chars)
    (evil-quickscope--update-seen-chars pos)))

(defun evil-quickscope--add-to-hl-chars ()
  "Add current hl-char pair to evil-quickscope--hl-chars list."
  (setq evil-quickscope--hl-chars (cons evil-quickscope--word-hl-chars evil-quickscope--hl-chars))
  (setq evil-quickscope--word-hl-chars (list 0 0)))

(defun evil-quickscope--update-seen-chars (pos)
  "Increment current char in evil-quickscope--seen-chars list and
updates hl-char pair."
  (setq evil-quickscope--seen-chars (evil-quickscope-increment-plist-char evil-quickscope--seen-chars evil-quickscope--char))
  (let ((occurences (plist-get evil-quickscope--seen-chars evil-quickscope--char))
        (hl-p (car evil-quickscope--word-hl-chars))
        (hl-s (cadr evil-quickscope--word-hl-chars)))
    (cond
     ((and (= occurences 1) (= hl-p 0))
      (setcar evil-quickscope--word-hl-chars pos))
     ((and (= occurences 2) (= hl-s 0))
      (setcar (cdr evil-quickscope--word-hl-chars) pos)))))

(defun evil-quickscope-get-highlighted-chars (start end)
  "Get highlighted chars and returns a list of first chars and second chars."
  (let ((evil-quickscope--hl-chars ())
        (evil-quickscope--word-hl-chars '(0 0))
        (evil-quickscope--seen-chars ())
        (direction (if (> end start) 1 -1))
        (pos start)
        (num-searches 0))
    (while (and (/= pos end)
                (or (eq evil-quickscope-search-max nil)
                    (< num-searches evil-quickscope-search-max)))
      (evil-quickscope--update-hl-chars pos)
      (setq pos (+ pos direction))
      (setq num-searches (1+ num-searches)))
    (evil-quickscope--add-to-hl-chars)
    evil-quickscope--hl-chars))

;;; Overlays
(defun evil-quickscope-apply-overlays-forward ()
  "Gets highlighted characters and apply overlays forward."
  (let* ((search-end (if evil-quickscope-cross-lines
                         (point-max) (line-end-position)))
         (hl-positions (evil-quickscope-get-highlighted-chars
                        (1+ (point)) search-end)))
    (evil-quickscope-apply-overlays hl-positions)))

(defun evil-quickscope-apply-overlays-backward ()
  "Gets highlighted characters and apply overlays backward."
  (let* ((search-end (if evil-quickscope-cross-lines
                         (point-min) (line-beginning-position)))
         (hl-positions (evil-quickscope-get-highlighted-chars
                        (1- (point)) search-end)))
    (evil-quickscope-apply-overlays hl-positions)))

(defun evil-quickscope-apply-overlays (hl-positions)
  "Applies quickscope overlays at specified positions."
  (dolist (hl-pair hl-positions)
    (cond
     ((> (car hl-pair) 0) ; First occurence of letter
      (evil-quickscope-set-overlay 'evil-quickscope-first-face (car hl-pair)))
     ((> (cadr hl-pair) 0) ; Second occurence of letter
      (evil-quickscope-set-overlay 'evil-quickscope-second-face (cadr hl-pair))))))

(defun evil-quickscope-set-overlay (face pos)
  "Sets face overlay at position."
  (overlay-put (make-overlay pos (1+ pos)) 'face face))

(defun evil-quickscope-remove-overlays ()
  "Remove all quickscope overlays from buffer."
  (dolist (face '(evil-quickscope-first-face
                  evil-quickscope-second-face))
    (remove-overlays nil nil 'face face)))

;;; Display updates
(defun evil-quickscope-update-overlays-bidirectional ()
  "Update overlays in both directions from point."
  (evil-quickscope-remove-overlays)
  (unless (and evil-quickscope-disable-in-comments
               (nth 4 (syntax-ppss)))
    (evil-quickscope-apply-overlays-forward)
    (evil-quickscope-apply-overlays-backward)))

(defun evil-quickscope-update-overlays-directional (is-forward)
  "Update overlay forward from point. If arg is nil, update backward."
  (evil-quickscope-remove-overlays)
  (if is-forward
      (evil-quickscope-apply-overlays-forward)
    (evil-quickscope-apply-overlays-backward)))

(defun evil-quickscope-update-overlays (is-forward)
  "Update overlays bidirectionally or directionally."
  (if evil-quickscope-bidirectional
      (evil-quickscope-update-overlays-bidirectional)
    (evil-quickscope-update-overlays-directional is-forward)))

(defun evil-quickscope-call-find (find-function)
  "Calls function and undo overlays if cancelled out."
  (unwind-protect
      (call-interactively find-function)
    (evil-quickscope-remove-overlays)))

(defun evil-quickscope-update-overlays-bidirectional-delayed ()
  "Update overlays bidirectionally with a delay."
  (when evil-quickscope-always-mode-timer
    (cancel-timer evil-quickscope-always-mode-timer))
  (setq evil-quickscope-always-mode-timer
        (run-at-time evil-quickscope-always-mode-delay nil
                     #'evil-quickscope-update-overlays-bidirectional)))

;;; Replacement evil-find-char* commands
;;;###autoload
(defun evil-quickscope-find-char ()
  "Move to the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char))

;;;###autoload
(defun evil-quickscope-find-char-backward ()
  "Move to the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-backward))

;;;###autoload
(defun evil-quickscope-find-char-to ()
  "Move before the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays t)
  (evil-quickscope-call-find 'evil-find-char-to))

;;;###autoload
(defun evil-quickscope-find-char-to-backward ()
  "Move before the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word."
  (interactive)
  (evil-quickscope-update-overlays nil)
  (evil-quickscope-call-find 'evil-find-char-to-backward))

;; Set evil properties of replacement commands
(evil-set-command-properties 'evil-quickscope-find-char
                             :type 'inclusive :jump t :keep-visual t :repeat 'motion)
(evil-set-command-properties 'evil-quickscope-find-char-backward
                             :type 'exclusive :jump t :keep-visual t :repeat 'motion)
(evil-set-command-properties 'evil-quickscope-find-char-to
                             :type 'inclusive :jump t :keep-visual t :repeat 'motion)
(evil-set-command-properties 'evil-quickscope-find-char-to-backward
                             :type 'exclusive :jump t :keep-visual t :repeat 'motion)

;;; Minor modes
;;;###autoload
(define-minor-mode evil-quickscope-always-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highglights always on."
  :init-value nil
  :lighter ""
  :keymap nil
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (remove-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional-delayed t)

  (when evil-quickscope-always-mode
    ;; Turn off quickscope-mode if on
    (when (bound-and-true-p evil-quickscope-mode)
      (evil-quickscope-mode 0))

    (add-hook 'post-command-hook 'evil-quickscope-update-overlays-bidirectional-delayed nil t)))

;;;###autoload
(define-globalized-minor-mode global-evil-quickscope-always-mode
  evil-quickscope-always-mode evil-quickscope-always-mode-turn-on
  "Global minor mode for evil-quickscope-always-mode.")

;;;###autoload
(defun evil-quickscope-always-mode-turn-on ()
  "Enable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-always-mode 1))

;;;###autoload
(defun evil-quickscope-always-mode-turn-off ()
  "Disable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-always-mode 0))

;;;###autoload
(define-minor-mode evil-quickscope-mode
  "Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highlights activate when f,F,t,T pressed."
  :init-value nil
  :lighter ""
  :keymap evil-quickscope-mode-map
  :global nil
  :group 'evil-quickscope

  (evil-quickscope-remove-overlays)
  (evil-normalize-keymaps)

  (when evil-quickscope-mode
    ;; Turn off quickscope-always-mode if on
    (when evil-quickscope-always-mode
      (evil-quickscope-always-mode 0))))

;;;###autoload
(define-globalized-minor-mode global-evil-quickscope-mode
  evil-quickscope-mode evil-quickscope-mode-turn-on
  "Global minor mode for evil-quickscope-mode.")

;;;###autoload
(defun evil-quickscope-mode-turn-on ()
  "Enable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-mode 1))

;;;###autoload
(defun evil-quickscope-mode-turn-off ()
  "Disable `evil-quickscope-mode'."
  (interactive)
  (evil-quickscope-mode 0))

(provide 'evil-quickscope)

;;; evil-quickscope.el ends here
