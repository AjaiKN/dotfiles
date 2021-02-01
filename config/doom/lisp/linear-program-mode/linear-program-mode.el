;;; linear-program-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
;;
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Created: September 20, 2024
;; Modified: September 20, 2024
;; Version: 0.0.1
;; Keywords: languages tools
;; Homepage: https://github.com/AjaiKN/linear-program-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; https://web.mit.edu/lpsolve/doc/CPLEX-format.htm

(defgroup linear-program nil
  ""
  :group 'languages)

(defvar linear-program-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; comments "\\ ...\n"
    (modify-syntax-entry ?\\ "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; spaces
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)

    ;; ;; parens
    ;; (modify-syntax-entry ?\( "()" table)
    ;; (modify-syntax-entry ?\) ")(" table)
    ;; (modify-syntax-entry ?\{ "(}" table)
    ;; (modify-syntax-entry ?\} "){" table)
    ;; (modify-syntax-entry ?\[ "(]" table)
    ;; (modify-syntax-entry ?\] ")[" table)

    ;; part of symbol
    (mapc (lambda (x)
            (modify-syntax-entry x "_" table))
          ;; cplex
          ;; "!\"#$%&()/,.;?@_`'{}|~"
          ;; with gurobi banned chars removed
          "!\"#$%&/,.;?@_`'{}|~")


    ;; punctuation
    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
          "><^=*[]-+:")

    table))

(defvar linear-program-keywords-cplex-section
  (list
   (rx bol "s.t.")
   (rx bol
       (or (or "minimize" "minimum" "min")
           (or "maximize" "maximum" "max")
           (or "bounds" "bound")
           (or (seq "subject" (+ space) "to") (seq "such" (+ space) "that") "st" "s.t.")
           (or "general" "generals" "gen")
           (or "binary" "binaries" "bin")
           (or "semi-continuous" "semi" "semis")
           "sos"
           "end")
       eow)))

;; https://web.mit.edu/lpsolve/doc/CPLEX-format.htm
;; TODO: https://www.ibm.com/docs/en/icos/20.1.0?topic=cplex-lp-file-format-algebraic-representation
(defvar linear-program-keywords-cplex
  (append
   linear-program-keywords-cplex-section
   (list
    (rx bow
        (or "inf" "infinity")
        eow))))

(defvar linear-program-keywords-gurobi-section
  (list
   (rx "s.t.")
   (rx bol
       (* space)
       (or (or "minimize" "minimum" "min")
           (or "maximize" "maximum" "max")
           "multi-objectives"
           "priority"
           "weight"
           "AbsTol"
           "RelTol"
           (or (seq "subject" (+ space) "to") (seq "such" (+ space) "that") "st" "s.t.")
           (seq "lazy" (+ space) "constraints")
           (seq "user" (+ space) "cuts")
           "bounds"
           (or "general" "generals" "gen")
           (or (or "binary" "binaries" "bin") (or "semi-continuous" "semi" "semis"))
           "sos"
           "PWLObj"
           (or (seq "general" (+ space) "constraint" (? "s"))
               "gencons"
               "g.c.")
           "Scenario"
           "end")
       eow)))

(defvar linear-program-keywords-gurobi
  (append
   linear-program-keywords-gurobi-section
   (list
    (rx bow
        (or (or "inf" "infinity")
            "free"
            "min" "max" "or" "and" "norm ""abs" "pwl"
            "poly" "pow" "exp" "expa" "log" "loga" "logistic" "sin" "cos" "tan")
        eow))))

(defvar linear-program-keywords-section
  (append
   linear-program-keywords-cplex-section
   linear-program-keywords-gurobi-section))

;; TODO: https://docs.mosek.com/latest/capi/lp-format.html
;; TODO: https://www.fico.com/fico-xpress-optimization/docs/dms2019-03/solver/optimizer/HTML/chapter10_sec_section102.html
;; https://web.mit.edu/lpsolve/doc/lp-format.htm

(defvar linear-program-keywords
  (append linear-program-keywords-cplex
          linear-program-keywords-gurobi))

(defun linear-program-font-lock-syntactic-face-function (_state)
  ""
  font-lock-comment-face)

(defun linear-program-font-lock-indent-line ()
  ""
  (let* ((line-begin (pos-bol))
         (current-level (save-excursion (beginning-of-line-text)
                                        (current-column)))
         (line (thing-at-point 'line))
         (line-empty-p (string-match-p (rx bos (* (or space "\n")) eos) line))
         (is-section-keyword (cl-loop for regex in linear-program-keywords-section
                                      if (string-match-p regex line)
                                      return t)))
    (cond
     (is-section-keyword (save-excursion (indent-line-to 0)))
     ((and (eq current-level 0)
           (cl-loop for regex in linear-program-keywords-section
                    if (save-excursion (goto-char (point-min))
                                       (search-forward-regexp regex line-begin t))
                    return t))
      (if line-empty-p
          (indent-line-to tab-width)
        (save-excursion (indent-line-to tab-width)))))))

(defcustom linear-program-electric-indent-after-keyword-p t
  "Whether to immediately indent after a keyword is typed.

This takes effect in `linear-program-mode' when
`electric-indent-mode' is enabled."
  :type 'boolean
  :group 'linear-program)

(defun linear-program-electric-indent-function (_char)
  ""
  (when linear-program-electric-indent-after-keyword-p
    (let* ((line (buffer-substring-no-properties (pos-bol) (point)))
           (is-section-keyword (cl-loop for regex in linear-program-keywords-section
                                        if (string-match-p regex line)
                                        return t)))
      is-section-keyword)))

;;;###autoload
(define-derived-mode linear-program-mode prog-mode
  "Linear Program"
  :group 'linear-program
  :syntax-table linear-program-mode-syntax-table
  (setq-local font-lock-defaults
              '(linear-program-keywords
                      nil
                      t
                      nil
                      nil
                (font-lock-syntactic-face-function . linear-program-font-lock-syntactic-face-function))
              comment-start "\\"
              comment-start-skip (rx (one-or-more "\\") (zero-or-more blank))
              indent-line-function #'linear-program-font-lock-indent-line)
  (add-hook 'electric-indent-functions #'linear-program-electric-indent-function nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lp\\'" . linear-program-mode))

(provide 'linear-program-mode)
;;; linear-program-mode.el ends here
