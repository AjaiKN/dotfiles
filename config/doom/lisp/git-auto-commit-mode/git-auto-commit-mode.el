;;; git-auto-commit-mode.el --- Emacs Minor mode to automatically commit and push  -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2013, 2014, 2015 Tom Willemse <tom@ryuslash.org>

;; Author: Tom Willemse <tom@ryuslash.org>
;; Created: Jan 9, 2012
;; Version: 4.7.0
;; Keywords: vc
;; URL: https://github.com/ryuslash/git-auto-commit-mode
;; Package-Requires: ((emacs "29.1"))

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; git-auto-commit-mode is an Emacs minor mode that tries to commit
;; changes to a file after every save.

;; When `git-auto-commit-automatically-push-p' is non-nil, it also tries to push
;; to the current upstream.

;; When `git-auto-commit-debounce-interval' is non-nil and set to a number
;; representing seconds, it will only perform Git actions at that
;; interval. That way, repeatedly saving a file will not hammer the
;; Git repository.

;;; Code:

(require 'subr-x)

(defgroup git-auto-commit nil
  "Customization options for `git-auto-commit-mode'."
  :group 'external)

(defcustom git-auto-commit-automatically-push-p nil
  "Automatically push after each commit.

If non-nil a git push will be executed after each commit."
  :tag "Automatically push"
  :group 'git-auto-commit
  :type 'boolean
  :risky t)
(make-variable-buffer-local 'git-auto-commit-automatically-push-p)

(defcustom git-auto-commit-automatically-add-new-files-p t
  "Should new (untracked) files automatically be committed to the repo?"
  :tag "Automatically add new files"
  :group 'git-auto-commit
  :type 'boolean)

(defcustom git-auto-commit-ask-for-summary-p nil
  "Ask the user for a short summary each time a file is committed?"
  :tag "Ask for a summary on each commit"
  :group 'git-auto-commit
  :type 'boolean)

(defcustom git-auto-commit-shell-and " && "
  "How to join commands together in the shell.

For fish shell, you want to customise this to: \" ; and \"
instead of the default."
  :tag "Join shell commands"
  :group 'git-auto-commit
  :type 'string)

;;;###autoload (put 'git-auto-commit-add-additional-flags 'risky-local-variable t)
(defcustom git-auto-commit-add-additional-flags ""
       "Flags to add to the git add command."
       :tag "git add flag"
       :group 'git-auto-commit
       :risky t
       :type 'string)

;;;###autoload (put 'git-auto-commit-commit-additional-flags 'risky-local-variable t)
(defcustom git-auto-commit-commit-additional-flags ""
    "Flags to add to the git commit command."
    :tag "git commit flag"
    :group 'git-auto-commit
    :risky t
    :type 'string)

(defcustom git-auto-commit-silent-message-p nil
    "Should git output be output to the message area?"
    :tag "Quiet message output"
    :group 'git-auto-commit
    :type 'boolean)


(defcustom git-auto-commit-debounce-interval nil
  "Debounce automatic commits to avoid hammering Git.

If non-nil a commit will be scheduled to occur that many seconds
in the future. Note that this uses Emacs timer functionality, and
is subject to its limitations."
  :tag "Debounce interval"
  :group 'git-auto-commit
  :type '(choice (number :tag "Interval in seconds")
                 (const :tag "Off" nil)))
(make-variable-buffer-local 'git-auto-commit-debounce-interval)

;;;###autoload (put 'git-auto-commit-default-message 'risky-local-variable t)
(defcustom git-auto-commit-default-message nil
  "Default message for automatic commits.

It can be:
- nil to use the default FILENAME
- a string which is used
- a function returning a string, called with FILENAME as
  argument, in which case the result is used as commit message"
  :tag "Default commit message"
  :group 'git-auto-commit
  :risky t
  :type '(choice (string :tag "Commit message")
                 (const :tag "Default: FILENAME" nil)
                 (function :tag "Function")))

(defun git-auto-commit-relative-file-name (filename)
  "Find the path to FILENAME relative to the git directory."
  (let* ((git-dir
          (string-trim-right
           (shell-command-to-string "git rev-parse --show-toplevel"))))
    (file-relative-name filename git-dir)))

(defun git-auto-commit-password (proc string)
  "Ask the user for a password when necessary.

PROC is the process running git.  STRING is the line that was
output by PROC."
  (let (ask)
    (cond
     ((or
       (string-match "^Enter passphrase for key '\\\(.*\\\)': $" string)
       (string-match "^\\\(.*\\\)'s password:" string))
      (setq ask (format "Password for '%s': " (match-string 1 string))))
     ((string-match "^[pP]assword:" string)
      (setq ask "Password:")))

    (when ask
      (process-send-string proc (concat (read-passwd ask nil) "\n")))))

(defun git-auto-commit-process-filter (proc string)
  "Check if PROC is asking for a password and promps the user if so.

STRING is the output line from PROC."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (git-auto-commit-password proc string))))

(defun git-auto-commit-process-sentinel (_proc status)
  "Report PROC change to STATUS."
  (unless git-auto-commit-silent-message-p
    (message "git %s" (substring status 0 -1))))

(defun git-auto-commit--commit-msg (filename)
  "Get a commit message.

Default to FILENAME."
  (let ((relative-filename (git-auto-commit-relative-file-name filename)))
    (if (not git-auto-commit-ask-for-summary-p)
        (if git-auto-commit-default-message
            (if (functionp git-auto-commit-default-message)
                (funcall git-auto-commit-default-message filename)
              git-auto-commit-default-message)
          relative-filename)
        (or git-auto-commit-default-message relative-filename)
      (read-string "Summary: " nil nil relative-filename))))

(defun git-auto-commit-commit (buffer)
  "Commit BUFFER's file to git."
  (let* ((buffer-file (buffer-file-name buffer))
         (filename (convert-standard-filename
                    (file-name-nondirectory buffer-file)))
         (commit-msg (git-auto-commit--commit-msg buffer-file))
         (default-directory (file-name-directory buffer-file))
         (add-command
          (unless (git-auto-commit--buffer-is-tracked buffer)
            (list "git add " git-auto-commit-add-additional-flags " -- " (shell-quote-argument filename)
                  git-auto-commit-shell-and)))
         (commit-command
          (list "git commit -m " (shell-quote-argument commit-msg) " " git-auto-commit-commit-additional-flags
                " -- " (shell-quote-argument filename))))
    (funcall (if git-auto-commit-silent-message-p
                 #'call-process-shell-command
               #'shell-command)
             (apply #'concat (append add-command commit-command)))))

(defun git-auto-commit-push (buffer)
  "Push commits to the current upstream.

This doesn't check or ask for a remote, so the correct remote
should already have been set up."
  ;; git-auto-commit-push is currently only called from git-auto-commit--after-save, where it is wrapped
  ;; in with-current-buffer, which should already take care of
  ;; default-directory. The explicit binding here is defensive, in case git-auto-commit-push
  ;; starts being used elsewhere.
  (let ((default-directory (file-name-directory (buffer-file-name buffer))))
    (let ((proc (start-process "git" "*git-auto-push*" "git" "push")))
      (set-process-sentinel proc #'git-auto-commit-process-sentinel)
      (set-process-filter proc #'git-auto-commit-process-filter))))

(defvar git-auto-commit--debounce-timers (make-hash-table :test #'equal))

(defun git-auto-commit--debounced-save ()
  (let* ((actual-buffer (current-buffer))
         (current-buffer-debounce-timer (gethash actual-buffer git-auto-commit--debounce-timers)))
    (unless current-buffer-debounce-timer
      (puthash actual-buffer
               (run-at-time git-auto-commit-debounce-interval nil
                            #'git-auto-commit--after-save
                            actual-buffer)
               git-auto-commit--debounce-timers))))

(defun git-auto-commit--buffer-is-tracked (buffer)
  "Check to see if BUFFER’s file is tracked in git."
  (let ((file-name (convert-standard-filename
                    (file-name-nondirectory
                     (buffer-file-name buffer)))))
    (not (string=
          (shell-command-to-string (concat "git ls-files -- " (shell-quote-argument file-name)))
          ""))))

(defun git-auto-commit--buffer-has-changes (buffer)
  "Check to see if there is any change in BUFFER."
  (let ((file-name (convert-standard-filename
                    (file-name-nondirectory
                     (buffer-file-name buffer)))))
    (not (string=
          (shell-command-to-string (concat "git diff -- " (shell-quote-argument file-name)))
          ""))))

(defun git-auto-commit--after-save (buffer)
  (unwind-protect
      (when (and (buffer-live-p buffer)
                 (or (and git-auto-commit-automatically-add-new-files-p
                          (not (git-auto-commit--buffer-is-tracked buffer)))
                     (git-auto-commit--buffer-has-changes buffer)))
        (git-auto-commit-commit buffer)
        (with-current-buffer buffer
          ;; with-current-buffer required here because git-auto-commit-automatically-push-p
          ;; is buffer-local
          (when git-auto-commit-automatically-push-p
            (git-auto-commit-push buffer))))
    (remhash buffer git-auto-commit--debounce-timers)))

(defun git-auto-commit-kill-buffer-hook ()
  (when (and git-auto-commit-debounce-interval
             git-auto-commit--debounce-timers
             (gethash (current-buffer) git-auto-commit--debounce-timers))
    (git-auto-commit--after-save (current-buffer))))

(add-hook 'kill-buffer-hook #'git-auto-commit-kill-buffer-hook)

(defun git-auto-commit-after-save-func ()
  "Commit the current file.

When `git-auto-commit-automatically-push-p' is non-nil also push."
  (if git-auto-commit-debounce-interval
      (git-auto-commit--debounced-save)
    (git-auto-commit--after-save (current-buffer))))

;;;###autoload
(define-minor-mode git-auto-commit-mode
  "Automatically commit any changes made when saving.

Optionally push the changes too."
  :group 'git-auto-commit
  :lighter " ga"
  (if git-auto-commit-mode
      (add-hook 'after-save-hook #'git-auto-commit-after-save-func t t)
    (remove-hook 'after-save-hook #'git-auto-commit-after-save-func t)))

(provide 'git-auto-commit-mode)

;;; git-auto-commit-mode.el ends here
