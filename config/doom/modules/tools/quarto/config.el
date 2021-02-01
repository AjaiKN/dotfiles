;;; tools/quarto/config.el -*- lexical-binding: t; -*-

;; NOTE: There's no Quarto major mode; it uses polymode
;; (https://polymode.github.io/), so there's a minor mode called
;; `poly-quarto-mode'.
(use-package! quarto-mode
  :mode (("\\.[qQ][mM][dD]" . poly-quarto-mode)
         ("\\.Rmd" . poly-quarto-mode))
  :config
  (defun +quarto/shift-enter ()
    (interactive)
    (save-current-buffer
      (save-selected-window
        (when (derived-mode-p 'python-mode)
          (condition-case _err
              (python-shell-switch-to-shell)
            (error
             (+python/open-repl)))
          (push-mark nil 'nomsg)
          (goto-char (point-max)))))
    (call-interactively #'polymode-eval-region-or-chunk)
    (call-interactively #'polymode-next-chunk-same-type)
    (ignore-errors
      (recenter)))
  (map! :map poly-quarto-mode-map
        :nviemg "S-<return>" #'+quarto/shift-enter
        :mn "[ [" #'polymode-previous-chunk
        :mn "] ]" #'polymode-next-chunk
        "C-c C-c" #'polymode-eval-buffer)
  (akn/advise-letf! polymode-eval-buffer (akn/redisplay-a)
    (define-advice pm-map-over-spans (:around (orig-fn fun &rest args) redisplay-a)
      (apply orig-fn
             (lambda (&rest args-inner)
               (prog1
                   (apply fun args-inner)
                 (redisplay)))
             args))))
