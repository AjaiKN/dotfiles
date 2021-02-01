;;; org-week-table.el --- https://emacs.stackexchange.com/a/59239 -*- lexical-binding: t; -*-
;;
;; Author: Lei Zhao
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This code is adapted from https://emacs.stackexchange.com/a/59239
;;  License: CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0/)
;;
;;; Code:

(require 'org-clock)

;; https://emacs.stackexchange.com/a/59239
(defun org-week-table--fmttm (tm)
  (format-time-string (org-time-stamp-format t t) tm))

;; https://emacs.stackexchange.com/a/59239
;;;###autoload
(defun org-dblock-write:weekly (params)
  (let ((file (or (plist-get params :file) (buffer-file-name)))
        (start (seconds-to-time
                (org-matcher-time (plist-get params :tstart))))
        (end (seconds-to-time (org-matcher-time (plist-get params :tend)))))
    (while (time-less-p start end)
      (let ((next-week (time-add start
                                 (date-to-time "1970-01-08T00:00Z")))
            (week-begin (line-beginning-position))
            (week-minutes 0)
            (total 0))
        (insert "\nWeekly Table from " (org-week-table--fmttm start) "\n")
        (insert "| Day of Week | Time |\n|-\n")
        (while (time-less-p start next-week)
          (let* ((next-day (time-add start (date-to-time "1970-01-02T00:00Z")))
                 (minutes
                  (with-current-buffer (find-file-noselect file)
                    (cadr (org-clock-get-table-data
                           file
                           (list :maxlevel 0
                                 :tstart (org-week-table--fmttm start)
                                 :tend (org-week-table--fmttm next-day)))))))
            (setq total (+ total minutes))
            (insert "|" (format-time-string "%a" start)
                    "|" (format "%d:%02d" (/ minutes 60) (mod minutes 60))
                    ;; (if (>= minutes 60) (format " (%d:%02d)" (/ minutes 60) (mod minutes 60)) "")
                    "|\n")
            (org-table-align)
            (cl-incf week-minutes minutes)
            (setq start next-day)))
        (insert (format "Total: %d:%02d\n" (/ total 60) (mod total 60)))
        (when (equal week-minutes 0)
          (delete-region week-begin (line-beginning-position)))))))

(provide 'org-week-table)
;;; org-week-table.el ends here
