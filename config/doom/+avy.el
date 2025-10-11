;;; +avy.el - Avy jump navigation configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'general)

(eval-when-compile
  (require 'doom-lib)
  (require 'doom))

(eval-and-compile
  (add-load-path! "./lisp"))

(eval-when-compile
  ;; (require 'doom-packages)
  (require 'akn-doom-use-package)
  ;; (require 'doom-modules)
  (require 'doom-keybinds)
  (require 'subr-x))

(eval-and-compile
  (setq! use-package-always-defer t))

(eval-and-compile
  (require 'akn))

;;; Stuff

(setq avy-all-windows t)
(setq avy-timeout-seconds 0.25)

;; https://karthinks.com/software/avy-can-do-anything/
(defun akn/avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun akn/avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    ;;(kill-whole-line))
    (call-interactively (if (modulep! :editor evil) #'evil-delete-whole-line #'kill-whole-line)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun akn/avy-action-copy-whole-line (pt)
  (if (modulep! :editor evil)
      (evil-save-state
        (save-mark-and-excursion
          (goto-char pt)
          ;; (cl-destructuring-bind (start . end)
          ;;     (bounds-of-thing-at-point 'line)
          ;;   (copy-region-as-kill start end))
          (call-interactively #'evil-visual-line)
          (call-interactively #'evil-yank)))
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end))))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun akn/avy-action-teleport-whole-line (pt)
  (akn/avy-action-kill-whole-line pt)
  (save-excursion
    ;; (yank)
    (if (modulep! :editor evil)
        (evil-paste-after nil)
      (yank)))
  t)

(defun akn/avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun akn/avy-action-docs (pt)
  (let ((thing (save-excursion
                 (goto-char pt)
                 (doom-thing-at-point-or-region))))
    (+lookup/documentation thing))
  t)

(defun akn/avy-action-definition (pt)
  (save-excursion
    (goto-char pt)
    (+lookup/definition (doom-thing-at-point-or-region)))
  t)

(setf (alist-get ?\; avy-dispatch-alist) #'akn/avy-action-embark
      ;; kill = cut
      (alist-get ?x avy-dispatch-alist)  #'avy-action-kill-stay
      (alist-get ?X avy-dispatch-alist)  #'akn/avy-action-kill-whole-line
      ;; copy
      (alist-get ?y avy-dispatch-alist)  #'avy-action-copy
      (alist-get ?Y avy-dispatch-alist)  #'akn/avy-action-copy-whole-line
      ;; yank = paste
      (alist-get ?p avy-dispatch-alist)  #'avy-action-yank
      (alist-get ?P avy-dispatch-alist)  #'avy-action-yank-whole-line
      ;; teleport = transpose
      (alist-get ?t avy-dispatch-alist)  #'avy-action-teleport
      (alist-get ?T avy-dispatch-alist)  #'akn/avy-action-teleport-whole-line
      ;; highlight
      (alist-get ?v avy-dispatch-alist)  #'akn/avy-action-mark-to-char
      ;; lookup
      (alist-get ?K avy-dispatch-alist)  #'akn/avy-action-docs
      (alist-get ?D avy-dispatch-alist)  #'akn/avy-action-definition)
