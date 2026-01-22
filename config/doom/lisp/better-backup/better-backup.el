;;; better-backup.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ajai Khatri Nelson
;;
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Created: June 03, 2025
;; Modified: June 03, 2025
;; Version: 0.0.1
;; Keywords: files local
;; Homepage: https://github.com/AjaiKN/better-backup
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; Also see
;; - https://codeberg.org/contrapunctus/async-backup/src/branch/production/async-backup.el
;; - https://github.com/conornash/backup-each-save/blob/master/backup-each-save.el
;; - https://github.com/Andersbakken/emacs-backup-file/blob/master/backup-file.el
;; - https://github.com/antham/git-backup/blob/master/git-backup.el
;; - mindstream
;; - Save remote files locally

(require 'files)
(require 'cl-lib)

;;;; Custom

(defgroup better-backup nil
  ""
  :group 'files)

(defcustom better-backup-directory (locate-user-emacs-file "better-backup/")
  "Where better-backup backups should go.

This can either be a directory or a list in the format of
`auto-save-file-name-transforms'."
  :group 'better-backup
  :type `(choice
          directory
          (repeat (list (regexp :tag "Regexp")
                        (string :tag "Replacement")
                        (choice
                         (const :tag "Uniquify" t)
                         ,@(mapcar (lambda (algo)
                                     (list 'const algo))
                                   (secure-hash-algorithms)))))))

(defcustom better-backup-buffer-defer nil
  ""
  :group 'better-backup
  :type 'boolean)

;;;; Choosing backup file names

(cl-defun better-backup--backup-file-name (&optional (file-or-buffer-name (current-buffer)))
  (when (bufferp file-or-buffer-name)
    (setq file-or-buffer-name (or (buffer-file-name file-or-buffer-name)
                                  (buffer-name file-or-buffer-name))))
  (cl-loop for i upfrom 1
           for ret = (let* ((time (format-time-string "%Y-%m-%d--%H-%M-%S-%6N--"))
                            (i-str (if (= i 1) "" (format "--%d" i)))
                            (path (files--transform-file-name
                                   (string-replace "~/" "" (abbreviate-file-name file-or-buffer-name))
                                   (if (listp better-backup-directory)
                                       better-backup-directory
                                     `((".*" ,(string-replace "\\" "\\\\" (file-name-as-directory better-backup-directory)) t)))
                                   time
                                   i-str))
                            (directory (file-name-directory path))
                            (nondirectory (file-name-nondirectory path)))
                       (if (length> nondirectory 200)
                           (let ((nondirectory (concat time
                                                       "--"
                                                       (substring (file-name-nondirectory file-or-buffer-name) -30)
                                                       "--"
                                                       (sha1 file-or-buffer-name)
                                                       i-str
                                                       ".txt")))
                             (file-name-concat directory nondirectory))
                         path))
           while (file-exists-p ret)
           finally return ret))

;;;; Backup primitives

(cl-defun better-backup--write-buffer-sync (&optional (file (better-backup--backup-file-name)))
  ""
  (mkdir (file-name-directory file) 'parents)
  (write-region nil nil file
                nil 0 nil 'excl))

(defvar better-backup--deferred-writes-out (cons nil nil))
(defvar better-backup--deferred-writes-in better-backup--deferred-writes-out)
(defun better-backup--deferred-writes-enqueue (thing)
  (let ((new-root (cons nil nil)))
    (setcar better-backup--deferred-writes-in thing)
    (setcdr better-backup--deferred-writes-in new-root)
    (setq better-backup--deferred-writes-in (cdr better-backup--deferred-writes-in))))
(defun better-backup--deferred-writes-dequeue ()
  (unless (and (null (car better-backup--deferred-writes-out))
               (null (cdr better-backup--deferred-writes-out)))
    (pop better-backup--deferred-writes-out)))
(cl-defun better-backup--write-buffer-deferred (&optional (file (better-backup--backup-file-name)))
  ;; TODO: start idle timer
  (let ((str
         (without-restriction
           (buffer-substring-no-properties (point-min) (point-max)))))
    (better-backup--deferred-writes-enqueue (cons str file))))
(defun better-backup--do-next-deferred-write ()
  (when-let* ((thing (better-backup--deferred-writes-dequeue)))
    (let ((str (car thing))
          (file (cdr thing)))
      (with-temp-buffer
        (insert str)
        (better-backup--write-buffer-sync file)))))

(cl-defun better-backup--write-buffer (&optional (file (better-backup--backup-file-name)))
  (funcall (if better-backup-buffer-defer
               #'better-backup--write-buffer-deferred
             #'better-backup--write-buffer-sync)
           file))

(cl-defun better-backup--copy-file-async (from &optional (to (better-backup--backup-file-name from)))
  ""
  (let ((process
         (make-process :name "better-backup"
                       :command `("cp" "-i" ,from ,to)
                       :connection-type 'pipe)))
    (process-send-eof process)))

(cl-defun better-backup--copy-file-sync (from &optional (to (better-backup--backup-file-name from)))
  ""
  (copy-file from to nil 'keep-mtime 'preserve-uid-gid 'preserve-permissions))

;;;; Main

(defvar-local better-backup--buffer-last-backup-tick nil
  "")
(defvar-local better-backup--buffer-local-saved-state nil
  "")

(cl-defun better-backup--buffer-backup-maybe (&optional (buffer (current-buffer)))
  ""
  (with-current-buffer (or buffer (current-buffer))
    (unless (equal (buffer-chars-modified-tick) better-backup--buffer-last-backup-tick)
      (better-backup--write-buffer)
      (setq better-backup--buffer-last-backup-tick (buffer-chars-modified-tick)))
    (unless (buffer-modified-p)
      (setq better-backup--buffer-local-saved-state
            (buffer-local-set-state backup-inhibited t)))))

;;;###autoload
(define-minor-mode better-backup-buffer-mode
  ""
  :group 'better-backup
  (if better-backup-buffer-mode
      (progn
        (if (buffer-modified-p)
            (better-backup--buffer-backup-maybe)
          (add-hook 'first-change-hook #'better-backup--buffer-backup-maybe nil 'local))
        (add-hook 'before-save-hook #'better-backup--buffer-backup-maybe nil 'local)
        (add-hook 'after-save-hook #'better-backup--buffer-backup-maybe 92 'local))
    (progn
      (remove-hook 'first-change-hook #'better-backup--buffer-backup-maybe 'local)
      (remove-hook 'before-save-hook #'better-backup--buffer-backup-maybe 'local)
      (remove-hook 'after-save-hook #'better-backup--buffer-backup-maybe 'local)
      (when better-backup--buffer-local-saved-state
        (buffer-local-restore-state better-backup--buffer-local-saved-state)
        (setq better-backup--buffer-local-saved-state nil)))))

(provide 'better-backup)
;;; better-backup.el ends here
