emacs-lisp-mode

(au & ";;;###autoload")

(lexical "-*- lexical-binding: t -*-")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " --- " p " " (i lexical) " -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)
(package (i header) r n n (i provide))
