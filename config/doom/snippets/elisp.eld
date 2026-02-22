emacs-lisp-mode

(au & ";;;###autoload" %)
(autodef & ";;;###autodef" %)

(lexical "-*- lexical-binding: t; -*-"
         :post (unless (doom-point-in-comment-p) (comment-line 1)))

(__package
 ";;; " (p (if buffer-file-name (file-name-nondirectory buffer-file-name) "") file) " --- " (p "Description") " -*- lexical-binding: t; -*-" n
 ";;" n
 ";; Copyright (C) " (format-time-string "%Y") " " (p (or user-full-name "") name) n
 ";;" n
 ";; Author: " name " <" (p (or user-mail-address "") email) ">" n
 ";; Maintainer: " name " <" email ">" n
 ";; Created: " (format-time-string "%B %d, %Y") n
 ";; Modified: " (format-time-string "%B %d, %Y") n
 ";; Version: 0.0.1" n
 ";; Keywords: " (mapconcat #'symbol-name (mapcar #'car finder-known-keywords) " ") n
 ";; Homepage: " (p (concat "https://github.com/" (cl-find-if-not #'string-empty-p (list (cdr (doom-call-process "git" "config" "github.user")) user-login-name)) "/" (file-name-base file))) n
 ";; Package-Requires: ((emacs \"24.3\"))" n
 ";;" n
 ";; This file is not part of GNU Emacs." n
 ";;" n
 ";;; Commentary:" n
 ";;" n
 ";;  " p n
 ";;" n
 ";;; Code:" n
 n
 r n
 n
 "(provide '" (file-name-base file) ")" n
 ";;; " file " ends here" n)

(__doom-module
 ";;; " (+file-templates-get-short-path) " -*- lexical-binding: t; -*-" n
 n
 q)

(__doom-packages
 ";; -*- no-byte-compile: t; lexical-binding: t; -*-"
 ";;; " (+file-templates-get-short-path) n
 n
 q)
