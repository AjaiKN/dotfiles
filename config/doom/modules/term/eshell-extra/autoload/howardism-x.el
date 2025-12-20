;;; term/eshell-extra/autoload/howardism-x.el -*- lexical-binding: t; -*-

;; https://howardism.org/Technical/Emacs/eshell-why.html
;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org (CC0)

(require 'dash)
(require 's)

(defun +eshell--x-cells (table row col)
    (let* ((newlines (rx (one-or-more (any "\n" "\r"))))
           (fields   (rx (one-or-more (any "\t" " "))))
           (rows     (split-string table newlines t))
           (line     (nth row rows)))
      (if col
        (nth col (split-string line fields t))
       line)))

(defun +eshell--x-match (text starter)
    (let ((words (split-string text nil t)))
      (--first (s-starts-with? starter it) words)))

;;;###autoload
(defun eshell/x (&rest args)
  "Return a cell of information from the previous command in an Eshell buffer.
The first ARGS is the line number (one-based), and the second
ARGS, if given, is the column where the fields are separated by
whitespace.

This allows a sequence of commands like, where you don't have to
copy/paste the output (if it is simple), for instance:

    $ ls
    ...
    $ ls -l { x 2 3 }

If the initial argument is a string instead of a number, then it
returns the first word that starts with that it."
  (let* ((arg1     (first args))
         (arg2     (second args))
         (contents (ha-eshell-last-output)))
    (cond
     ((numberp arg1) (+eshell--x-cells contents arg1 arg2))
     ((stringp arg1) (+eshell--x-match contents arg1))
     (t              contents))))
