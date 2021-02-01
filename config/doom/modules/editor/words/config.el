;;; editor/words/config.el -*- lexical-binding: t; -*-

;; (akn/make-overrider! #'forward-evil-WORD)
;; (define-minor-mode akn/evil-WORD-symbol-mode
;;   ""
;;   :group 'akn
;;   :global nil
;;   (akn/mode-set akn/evil-WORD-symbol-mode
;;     akn/overrider/forward-evil-WORD-function #'forward-evil-symbol))
;; (define-globalized-minor-mode akn/global-evil-WORD-symbol-mode
;;   akn/evil-WORD-symbol-mode akn/evil-WORD-symbol-mode
;;   :predicate `(,@akn/lisp-like-modes prog-mode)
;;   :group 'akn)
;; (akn/global-evil-WORD-symbol-mode)

(define-advice forward-evil-word (:around (fn &rest args) +words)
  (apply
   (if (bound-and-true-p superword-mode)
       #'forward-evil-symbol
     fn)
   args))
(define-advice forward-evil-WORD (:around (fn &rest args) +words)
  (apply
   (if (bound-and-true-p subword-mode)
       #'forward-evil-symbol
     fn)
   args))

(defun +words/toggle-subword-superword ()
  "Toggle between `subword-mode' and `superword-mode'."
  (interactive)
  (require 'subword)
  (funcall-interactively
   (if subword-mode #'superword-mode #'subword-mode)))
(defun +words/regularword-mode ()
  "Disable `subword-mode' and `superword-mode'."
  (interactive)
  (when (bound-and-true-p subword-mode) (subword-mode -1))
  (when (bound-and-true-p superword-mode) (superword-mode -1)))

(use-package! subword
  :defer-incrementally t
  :ghook ('doom-first-input-hook #'global-subword-mode))
