;;; input/reverse-im/config.el -*- lexical-binding: t; -*-

(require 'akn)

(defcustom +reverse-im-input-source-alist '()
  "Mapping from Mac input sources to Emacs input methods."
  :group 'akn
  :type '(alist :key-type string :value-type mule-input-method-string)
  :risky t)

(use-package! reverse-im
  :defer t
  :defer-incrementally (quail cl-lib seq subr-x)
  :init
  (defun +reverse-im-maybe-reverse-im-mode ()
    (when (fboundp 'mac-input-source)
      (if-let* ((input-source (ignore-errors (mac-input-source)))
                (input-method (alist-get input-source +reverse-im-input-source-alist nil nil #'equal)))
          (progn
            (setq! reverse-im-input-methods (list input-method))
            (reverse-im-mode))
        (when (bound-and-true-p reverse-im-mode)
          (reverse-im-mode -1)))))
  (add-hook 'window-setup-hook #'+reverse-im-maybe-reverse-im-mode)
  (add-hook 'doom-first-input-hook #'+reverse-im-maybe-reverse-im-mode)
  (add-hook 'mac-selected-keyboard-input-source-change-hook #'+reverse-im-maybe-reverse-im-mode)
  (add-function :after after-focus-change-function #'+reverse-im-maybe-reverse-im-mode))
