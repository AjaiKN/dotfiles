;;; elisp-indent-docstrings.el --- Make elisp docstrings appear to be indented -*- lexical-binding: t; -*-
;;
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Homepage: https://lists.nongnu.org/archive/html/emacs-devel/2022-03/msg00261.html
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This code is adapted from https://lists.nongnu.org/archive/html/emacs-devel/2022-03/msg00261.html:
;;
;;     As you probably know, the convention in ELisp is to write the content of
;;     docstrings "flush left", since any leading space will be considered part of
;;     the actual docstring which then typically leads to ugly results when the
;;     docstring is displayed *Help* by `C-h o`.
;;
;;     See for example: https://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp
;;
;;     For those who can't get used to it, here's a minor mode which will indent the
;;     content of docstrings but only on the display, without affecting the actual
;;     buffer.
;;
;;             Stefan
;;
;;; Code:

;;;###autoload
(define-minor-mode elisp-indent-docstrings-mode
  "If non-nil, docstrings are displayed with extra indentation.

Copied and modified (to make it a local mode, not global) from
https://lists.nongnu.org/archive/html/emacs-devel/2022-03/msg00261.html"
  :group 'akn
  (when elisp-indent-docstrings-mode
    (elisp-indent-docstrings--add-font-lock-rule)))

;; TODO: maybe add a globalized minor mode

(defun elisp-indent-docstrings--add-font-lock-rule ()
  "Add a font-lock rule in this buffer to display docstrings indented."
  (font-lock-add-keywords nil '((elisp-indent-docstrings--indent-docstrings)) 'append)
  (font-lock-flush)
  (push 'line-prefix font-lock-extra-managed-props))

(defun elisp-indent-docstrings--indent-docstrings (limit)
  "Display docstrings with extra indentation in this buffer."
  (when elisp-indent-docstrings-mode
    (let ((pos nil))
      (while (and (< (point) limit)
                  (setq pos (text-property-any (point) limit
                                               'face 'font-lock-doc-face)))
        (goto-char pos)
        (let* ((ppss (syntax-ppss))
               (start (or (nth 8 ppss) pos))
               (indent (save-excursion
                         (goto-char start)
                         (when (and (eq (char-after) ?\")
                                    (not (eq (char-after (1+ (point))) ?\\)))
                           (1+ (current-column)))))
               (display (when indent (concat ;; "\n"
                                      (make-string indent ?\s))))
               (end (or (text-property-not-all (point) limit
                                               'face 'font-lock-doc-face)
                        limit)))
          (if (not display)
              (goto-char end)
            (while (re-search-forward "^." end 'move)
              (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                                 'line-prefix display)))))))
  nil)

(provide 'elisp-indent-docstrings)
;;; elisp-indent-docstrings.el ends here
