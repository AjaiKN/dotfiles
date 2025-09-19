;;; editor/smart-tabs/config.el -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'akn))

;; TODO: get `dtrt-indent' to be able to guess whether to enable `smart-tabs-mode'

(use-package! smart-tabs-mode
  :defer t
  :config
  (setq-hook! 'smart-tabs-mode-hook
    evil-indent-convert-tabs nil)

  (defmacro +smart-tabs-unadvice (function _offset)
    "Convenience function for removing the advice added by `smart-tabs-advice'."
    `(with-demoted-errors "Failed to remove advice: %S"
       (ad-remove-advice #',function 'around 'smart-tabs)))
  (akn/rotate-symbols! 'emacs-lisp-mode-hook "smart-tabs-advice" "+smart-tabs-unadvice")

  (defvar sh-basic-offset)
  (smart-tabs-advice sh-basic-indent-line sh-basic-offset)
  (smart-tabs-advice sh-smie--indent-continuation sh-basic-offset)

  (define-advice smie-indent-line (:around (orig-fun &rest args) +smart-tabs)
    "This advice is similar to the one that would be added by
`smart-tabs-advice'. But `smie-indent-line' is used by multiple
modes, so there are some complications."
    (cond
     ((and smart-tabs-mode indent-tabs-mode
           (eq tab-width
               (or (funcall smie-rules-function :elem 'basic)
                   smie-indent-basic)))
      (save-excursion
        (beginning-of-line)
        (while (looking-at "\t*\\( +\\)\t+")
          (replace-match "" nil nil nil 1)))
      (setq tab-width tab-width)

      ;; I took this variable list from `dtrt-indent-hook-mapping-list'
      (akn/letf! ((tab-width fill-column)
                  (smie-indent-basic fill-column)
                  (sh-basic-offset fill-column)
                  (ruby-indent-level fill-column)
                  (enh-ruby-indent-level fill-column)
                  (crystal-indent-level fill-column)
                  (css-indent-offset fill-column)
                  (rust-indent-offset fill-column)
                  (rustic-indent-offset fill-column)
                  (scala-indent:step fill-column)

                  (+smart-tabs--old-smie-rules-function smie-rules-function)
                  ;; When the `smie-rules-function' is called with arguments :elem and 'basic,
                  ;; it's supposed to return the basic offset.
                  (smie-rules-function
                   (lambda (method arg &rest rest)
                     (if (and (eq method :elem) (eq arg 'basic))
                         fill-column
                       (apply +smart-tabs--old-smie-rules-function method arg rest)))))
        (unwind-protect
            (progn (apply orig-fun args)))))

     (t (apply orig-fun args)))))
