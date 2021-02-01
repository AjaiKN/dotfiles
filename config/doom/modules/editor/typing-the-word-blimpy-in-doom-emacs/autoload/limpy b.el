;;; editor/typing-the-word-blimpy-in-doom-emacs/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

(defvar +typing-the-word-blimpy-in-doom-emacs--the-original-evil-state-before-typing-the-word-blimpy
  nil)

;;;###autodef
(defun +typing-the-word-blimpy-in-doom-emacs--switch-to-evil-insert-state-h ()
  (doom-log "recording the old evil state")
  (setq-local +typing-the-word-blimpy-in-doom-emacs--the-original-evil-state-before-typing-the-word-blimpy evil-state)
  (doom-log "switching to insert state")
  (if evil-move-beyond-eol
      (evil-insert 1)
    (evil-append 1)))

;;;###autoload
(defun +typing-the-word-blimpy-in-doom-emacs--switch-back-to-the-original-evil-state-a (chars &rest _)
  (when (and (null chars) ; we're done typing all the letters
             +typing-the-word-blimpy-in-doom-emacs--the-original-evil-state-before-typing-the-word-blimpy)
    (evil-change-state +typing-the-word-blimpy-in-doom-emacs--the-original-evil-state-before-typing-the-word-blimpy)))
