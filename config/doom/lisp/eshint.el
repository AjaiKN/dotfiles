;;; eshint.el --- eshell + comint (WIP) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ajai Khatri Nelson
;;
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Created: April 13, 2025
;; Modified: April 13, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/AjaiKN/eshint
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; eshell + comint
;;
;;; Code:

(require 'ielm)
(require 'eshell)

(defvar ielm-input)

(defun eshint-is-whitespace-or-comment (&rest _)
  nil)

(defun eshint-eval-input (input-string &optional _for-effect)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  (let ((output "")
        (dir default-directory))
    (with-temp-buffer
      (setq default-directory dir)
      (eshell-command input-string t)
      (setq output (buffer-string))
      (setq dir default-directory))
    ;; (setq output (eshell-command-result input-string))
    (unless (string-match-p (rx "\n" eos) output)
      (setq output (concat output "\n")))
    (setq output (concat output ielm-prompt-internal))
    (comint-output-filter (ielm-process) output)))

(defun eshint-send-input (&optional for-effect)
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let (ielm-input)                     ; set by ielm-input-sender
    (comint-send-input)                 ; update history, markers etc.
    (eshint-eval-input ielm-input for-effect)))

(defun eshint-return (&optional for-effect)
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `eshint-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if t ;eshint-dynamic-return
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (ielm-pm)
                                   (point)))))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (eshint-send-input for-effect)
          (when (and t ; eshint-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (ielm-pm))
              (newline 1)))
          (newline-and-indent)))
    (newline)))

(defun eshint-return-for-effect ()
  "Like `eshint-return', but do not print the result."
  (interactive)
  (eshint-return t))

(defvar-keymap eshint-mode-map
  ;; "<remap> <ielm-tab>"                    #'eshint-tab
  "<remap> <ielm-return>"                 #'eshint-return
  "<remap> <ielm-return-for-effect>"      #'eshint-return-for-effect
  "<remap> <ielm-send-input>"             #'eshint-send-input)
  ;; "<remap> <ielm-change-working-buffer>"  #'eshint-change-working-buffer
  ;; "<remap> <ielm-display-working-buffer>" #'eshint-display-working-buffer
  ;; "<remap> <ielm-print-working-buffer>"   #'eshint-print-working-buffer)

(define-derived-mode eshint-mode inferior-emacs-lisp-mode "EshInt"
  ""
  (setq-local comment-start "#"))

(provide 'eshint)
;;; eshint.el ends here
