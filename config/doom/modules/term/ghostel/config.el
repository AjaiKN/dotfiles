;;; term/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  :defer t
  :defer-incrementally t
  :config
  (defun +ghostel--kkp-app-p ()
    nil)
  (defun +ghostel--dumb-key (&rest args)
    `(menu-item "" nil :filter
      ,(lambda (&optional _)
         (if (+ghostel--kkp-app-p)
             #'ghostel--send-event
           (cmd! (apply #'ghostel-send-key args))))))
  (map! :map (ghostel-semi-char-mode-map ghostel-char-mode-map)
        :nviemorg "s-<left>"      (+ghostel--dumb-key "a" "ctrl")
        :nviemorg "s-<right>"     (+ghostel--dumb-key "e" "ctrl")
        :nviemorg "s-<backspace>" (+ghostel--dumb-key "u" "ctrl")
        :nviemorg "M-<left>"      (+ghostel--dumb-key "b" "meta")
        :nviemorg "M-<right>"     (+ghostel--dumb-key "f" "meta")))

(use-package! evil-ghostel
  :when (modulep! :editor evil)
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; TODO: disable xterm-color before enabling these:

;; (use-package! ghostel-compile
;;   :defer t
;;   :after-call compilation-start compile recompile
;;   :init
;;   (after! (:and compile ghostel) (require 'ghostel-compile))
;;   (define-advice compilation-start (:around (fn &rest args) +ghostel-compile-global-a -95)
;;     "Lazy load ghostel-compile when compilation-start is first called."
;;     (if (featurep 'ghostel-compile)
;;         (apply fn args)
;;       (require 'ghostel-compile)
;;       (apply #'compilation-start args)))
;;   (map! [remap compile]   #'ghostel-compile
;;         [remap recompile] #'ghostel-recompile)
;;   :config
;;   (advice-remove #'compilation-start 'compilation-start@+ghostel-compile-global-a)
;;   (ghostel-compile-global-mode)
;;   (when (modulep! :ui popup +defaults)
;;     (set-popup-rule! "^\\*ghostel-compil"
;;       :vslot -2 :size 0.3 :autosave t :quit t :ttl 0)))

;; (use-package! ghostel-eshell
;;   :when (modulep! :term eshell)
;;   :ghook ('eshell-load-hook #'ghostel-eshell-visual-command-mode))

;; (use-package! ghostel-comint
;;   :ghook 'shell-mode-hook

;;   :after-call comint-mode-hook
;;   :config
;;   (ghostel-comint-global-mode 1)

;;   (add-hook! '(shell-mode-hook ghostel-comint-mode-hook)
;;     (font-lock-mode -1)
;;     (setq-local font-lock-function #'ignore)))
