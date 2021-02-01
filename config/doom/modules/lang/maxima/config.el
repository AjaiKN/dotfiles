;;; lang/maxima/config.el -*- lexical-binding: t; -*-

(use-package maxima
  :defer t
  :init
  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; (setq maxima-display-maxima-buffer nil)
  (add-to-list 'auto-mode-alist
               (cons (rx ".mac" eos) #'maxima-mode))
  (add-to-list 'interpreter-mode-alist
               (cons (rx "maxima") #'maxima-mode)))

(use-package! company-maxima
  :when (modulep! :completion corfu)
  :defer t
  :autoload (company-maxima-libraries company-maxima-symbols akn/maxima-symbols-capf akn/maxima-libraries-capf)
  :after maxima
  :config
  (defalias 'akn/maxima-symbols-capf   (cape-company-to-capf #'company-maxima-symbols))
  (defalias 'akn/maxima-libraries-capf (cape-company-to-capf #'company-maxima-libraries))
  (add-hook! '(maxima-mode-hook maxima-inferior-mode-hook)
    (defun akn/add-maxima-capf ()
      (add-hook! 'completion-at-point-functions :local
                 #'akn/maxima-symbols-capf
                 #'akn/maxima-libraries-capf))))

(use-package! company-maxima
  :when (modulep! :completion company)
  :defer t
  :autoload (company-maxima-libraries company-maxima-symbols)
  :init
  (after! company
    (add-to-list 'company-backends '(company-maxima-symbols company-maxima-libraries))))
