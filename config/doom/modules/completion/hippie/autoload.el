;;; completion/hippie/autoload.el -*- lexical-binding: t; -*-


;; https://tecosaur.github.io/emacs-config/#suffix-stripping
(defun +hippie--he-subst-suffix-overlap (ins rem)
  "The longest suffix of the string INS that is a prefix of REM.
This is intended to be used when INS is a newly inserted string and REM is the
remainder of the line, to allow for handling potentially duplicated content."
  (let ((len (min (length ins) (length rem))))
    (while (and (> len 0)
                (not (eq 't (compare-strings ins (- len) nil rem 0 len))))
      (setq len (1- len)))
    len))
;;;###autoload
(defun +hippie--he-substitute-string-a (args)
  "Filter ARG list for `he-substitute-string', truncating duplicated suffix.
ARGS is the raw argument list (STRING &optional TRANS-CASE)."
  (pcase-let* ((`(,ins &optional ,trans-case) args)
               (rem (save-excursion
                      (goto-char (marker-position he-string-end))
                      (buffer-substring-no-properties
                       (point) (line-end-position))))
               (ov (+hippie--he-subst-suffix-overlap ins rem)))
    (when (>= ov 0)
      (setq ins (substring ins 0 (- (length ins) ov))))
    (list ins trans-case)))
