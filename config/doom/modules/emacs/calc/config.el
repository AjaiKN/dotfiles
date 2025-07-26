;;; emacs/calc/config.el -*- lexical-binding: t; -*-

;; I don't think `calc' uses `display-buffer', so `set-popup-rule!' has no effect:
;; https://yhetil.org/emacs-bugs/87363k8kw0.fsf@gnus.org/T/#t
(after! calc
  (defvar akn/calc-escape-quit-self t)
  (defvar akn/calc-escape-quit-others t)
  (defvar akn/calc-edit-escape-quit-self t)
  (defvar akn/calc-edit-escape-quit-others t)
  (defvar akn/calc-embedded-escape-quit-self nil)
  (defvar akn/calc-embedded-escape-quit-others nil)
  (add-hook! 'doom-escape-hook :append
    (defun akn/quit-calc-h ()
      (unless calc-standalone-flag
        (when (or (and akn/calc-escape-quit-self (derived-mode-p 'calc-mode))
                  (and akn/calc-edit-escape-quit-self (derived-mode-p 'calc-edit-mode))
                  (and akn/calc-embedded-escape-quit-self calc-embedded-info)
                  (let ((is-any-calc-window-visible nil))
                    (walk-windows (lambda (window)
                                    (with-selected-window window
                                      (when (or (and akn/calc-escape-quit-others (derived-mode-p 'calc-mode))
                                                (and akn/calc-edit-escape-quit-others (derived-mode-p 'calc-edit-mode))
                                                (and akn/calc-embedded-escape-quit-others calc-embedded-info))
                                        (setq is-any-calc-window-visible t)))))
                    is-any-calc-window-visible))
          (calc-quit)
          t))))

  (setq!
   calc-settings-file (file-name-concat doom-user-dir "calc.el")
   ;; By default, multiplication has precedence over division in calc for some reason.
   ;; Let's make it follow regular order of operations (multiplication and division have equal precedence).
   calc-multiplication-has-precedence nil
   calc-make-windows-dedicated t
   ;; calc-incomplete-algebraic-mode t
   calc-algebraic-mode t
   calc-angle-mode 'deg
   calc-display-trail nil
   calc-window-height 22
   calc-dispatch-help t
   calc-ensure-consistent-units t ; not sure what this does exactly, but seems safer to enable
   calc-kill-line-numbering nil))

(use-package! calc-transient
  :defer t
  :commands (calc-transient)
  :ghook 'calc-mode-hook)
