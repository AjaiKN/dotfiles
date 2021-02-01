;;; boring-processes.el --- Is this process just a shell? -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
;;
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Created: August 13, 2024
;; Version: 0.0.1
;; Keywords: convenience processes terminals
;; Homepage: https://github.com/AjaiKN/boring-processes
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Ever get annoyed when Emacs asks you whether it's okay to kill a process, and
;; it turns out it's just a boring bash shell with no subprocesses? This package
;; helps you identify the boring processes and mark them as safe to kill.
;;
;;; Code:

(require 'cl-lib)

(defgroup boring-processes nil
  ""
  :group 'processes)

(defcustom boring-processes-shell-regexp
  (rx (* whitespace)
      (? (* (not whitespace)) (or "/" "\\"))
      (or
       "ash"
       "bash"
       "ch"
       "csh"
       "dash"
       "fish"
       "ksh"
       "nu"
       "pwsh"
       "rc"
       "sash"
       "scsh"
       "sh"
       "tcsh"
       "zsh"
       ;; windows cmd.exe
       (seq (? "start") (+ whitespace) (? (* (not whitespace)) (or "/" "\\")) "cmd" (? ".exe"))
       "start"))
  ""
  :type 'regexp
  :group 'boring-processes)

(defcustom boring-processes-shell-arguments-regexp
  (rx (* (+ whitespace)
         ;; (or "-i" "--noediting")
         (* (seq "-" (* (any alphanumeric "-_")))))
      (* whitespace))
  ""
  :type 'regexp
  :group 'boring-processes)

(defcustom boring-processes-name-regexp
  (rx "applescript-fullscreen")
  ""
  :type 'regexp
  :group 'boring-processes)

;; https://old.reddit.com/r/emacs/comments/uy7h0d/vterm_do_not_query_on_exit_flag_if_there_is_no/
(defun boring-processes-process-shell-p (process)
  ""
  (if-let* ((cmd (process-command process))
            (cmd-str (string-join cmd " ")))
    (string-match-p (rx (regexp boring-processes-shell-regexp)
                        (regexp boring-processes-shell-arguments-regexp)
                        eos)
                    cmd-str)))

(defun boring-processes-just-a-shell-p (&optional process)
  ""
  (when (null process) (setq process (get-buffer-process (current-buffer))))
  (or (and (stringp (process-name process))
           (string-match-p boring-processes-name-regexp (process-name process)))
      (and (boring-processes-process-shell-p process)
           (let ((child (process-running-child-p process)))
             (or (not child)
                 (string-match-p (rx (regexp boring-processes-shell-regexp) eos)
                                 (alist-get 'comm (process-attributes child))))))))

(defun boring-processes-not-just-a-shell-p (process)
  ""
  (not (boring-processes-just-a-shell-p process)))

(defun boring-processes-just-a-shell-processes ()
  ""
  (seq-filter #'boring-processes-just-a-shell-p (process-list)))

(defun boring-processes-not-just-a-shell-processes ()
  ""
  (seq-filter #'boring-processes-not-just-a-shell-p (process-list)))

;;;###autoload
(defun boring-processes-mark-safe-advice (fn &rest args)
  ""
  (let* ((processes (boring-processes-just-a-shell-processes))
         (process-flags-before (mapcar #'process-query-on-exit-flag processes)))
    (unwind-protect
        (progn
          (cl-loop for process in processes
                   do (set-process-query-on-exit-flag process nil))
          (apply fn args))
      ;; restore original flags
      (cl-loop for process in processes
               for process-flag-before in process-flags-before
               when (and (processp process) (process-live-p process))
               do (set-process-query-on-exit-flag process process-flag-before)))))

;;;###autoload
(defun boring-processes-kill-current-buffer ()
  ""
  (interactive)
  (let* ((process (get-buffer-process (current-buffer)))
         (should-unflag (and process
                             (process-query-on-exit-flag process)
                             (boring-processes-just-a-shell-p process))))
    (when should-unflag
      (set-process-query-on-exit-flag process nil))
    (unwind-protect
        (kill-current-buffer)
      (when (and should-unflag (processp process) (process-live-p process))
        (set-process-query-on-exit-flag process t)))))

(provide 'boring-processes)
;;; boring-processes.el ends here
