;;; term/eshell-extra/+howardism-last-results.el -*- lexical-binding: t; -*-

;; https://howardism.org/Technical/Emacs/eshell-why.html
;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#last-results (CC0)

(defvar +eshell--output (make-ring 10)
  "A ring (looped list) storing history of eshell command output.")

;;;###autoload
(defun +eshell--store-last-output-h ()
  "Store the output from the last eshell command.
Called after every command by connecting to the `eshell-post-command-hook'."
  (let ((output
         (buffer-substring-no-properties eshell-last-input-end eshell-last-output-start)))
    (ring-insert +eshell--output output)))

;;;###autoload
(defun eshell/output (&rest args)
  "Return an eshell command output from its history.

The first argument is the index into the historical past, where
`0' is the most recent, `1' is the next oldest, etc.

The second argument represents the returned output:
 * `text' :: as a string
 * `list' :: as a list of elements separated by whitespace
 * `file' :: as a filename that contains the output

If the first argument is not a number, it assumes the format
to be `:text'.
"
  (let (frmt element)
    (cond
     ((> (length args) 1)  (setq frmt (cadr args)
                                 element (car args)))
     ((= (length args) 0)  (setq frmt "text"
                                 element 0))
     ((numberp (car args)) (setq frmt "text"
                                 element (car args)))
     ((= (length args) 1)  (setq frmt (car args)
                                 element 0)))

    (if-let ((results (ring-ref +eshell--output (or element 0))))
        (cl-case (string-to-char frmt)
          (?l     (split-string results))
          (?f     (+eshell--store-file-output results))
          (otherwise (s-trim results)))
      "")))

(defun +eshell--store-file-output (results)
  "Writes the string, RESULTS, to a temporary file and returns that file name."
  (let ((filename (make-temp-file "+eshell--")))
    (with-temp-file filename
      (insert results))
    filename))

(defun +eshell--output (format-type indices)
  "Wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (if indices
      (eshell/output (string-to-number (caar indices)) format-type)
    (eshell/output 0 format-type)))

(defun +eshell-output-text (&optional indices &rest _)
  "A _text_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (+eshell--output "text" indices))

(defun +eshell-output-list (&optional indices &rest _)
  "A _list_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (+eshell--output "list" indices))

(defun +eshell-output-file (&optional indices &rest _)
  "A _file_ wrapper around `eshell/output' for the `eshell-variable-aliases-list'."
  (+eshell--output "file" indices))
