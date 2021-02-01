;;; completion/fuzzy/config.el -*- lexical-binding: t; -*-

(require 'akn)

(defvar fussy-score-fn)
(setq fussy-score-fn
      (cond
       ((modulep! +flx-rs)        #'fussy-flx-rs-score)
       ((modulep! +flx)           #'flx-score)
       ((modulep! +fzf-native)    #'fussy-fzf-native-score)
       ((modulep! +fuz-bin)       #'fussy-fuz-bin-score)
       ((modulep! +fuz)           #'fussy-fuz-score)
       ((modulep! +liquidmetal)   #'fussy-liquidmetal-score)
       ((modulep! +sublime-fuzzy) #'fussy-sublime-fuzzy-score)
       ((modulep! +hotfuzz)       #'fussy-hotfuzz-score)
       (t                         #'flx-score)))

;; my results (https://github.com/axelf4/emacs-completion-bench)
;; basic:                                                 0.100273
;; hotfuzz:                                               0.300570
;; orderless:                                             0.466877
;; (fussy fussy-fuz-bin-score . fussy-filter-default):    0.987570
;; substring:                                             1.210470
;; (fussy fussy-fzf-native-score . fussy-filter-default): 1.211180
;; (fussy flx-score . fussy-filter-default):              3.093376
;; (fussy fussy-fuz-bin-score . fussy-filter-flex):       3.589512
;; (fussy fussy-fzf-native-score . fussy-filter-flex):    3.701862
;; flex:                                                  4.070264
;; (fussy flx-score . fussy-filter-flex):                 4.077692
;; (fussy flx-rs-score . fussy-filter-flex):              4.889162
;; (fussy flx-rs-score . fussy-filter-default):           6.917106

(use-package! fussy
  :defer 0.5
  :defer-incrementally (flx)
  :config
  ;; https://github.com/jojojames/fussy?tab=readme-ov-file#my-configuration
  (setq! fussy-compare-same-score-fn #'fussy-histlen->strlen<)
  (setq! fussy-propertize-fn #'fussy-propertize-common-part)

  ;; https://github.com/jojojames/fussy?tab=readme-ov-file#scoring-backends
  (shut-up
    (when (eq fussy-score-fn 'fussy-flx-rs-score) (flx-rs-load-dyn))
    (when (eq fussy-score-fn 'fussy-fzf-native-score) (fzf-native-load-dyn))
    (when (eq fussy-score-fn 'fussy-fuz-bin-score) (fuz-bin-load-dyn))
    (when (eq fussy-score-fn 'fussy-sublime-fuzzy-score) (sublime-fuzzy-load-dyn)))

  ;; https://github.com/jojojames/fussy?tab=readme-ov-file#filtering-choices
  ;; NOTE: Confusingly, `fussy-filter-default' is not actually the default (the default is `fussy-filter-flex').
  ;; `fussy-filter-default' seems like it really is buggy.
  (setq! fussy-filter-fn #'fussy-filter-flex)
  (use-package! orderless :defer t :if (memq fussy-filter-fn '(fussy-filter-orderless fussy-filter-orderless-flex)) :autoload (orderless-filter)))

(defadvice! akn/fussy-ignore-spaces-a (args)
  :filter-args #'fussy-all-completions
  :filter-args #'fussy-try-completions
  (cl-destructuring-bind (string table pred point) args
    (list (if (stringp string)
              (string-replace "~" "" (string-replace " " "" string))
            string)
          table
          pred
          (if (and (stringp string) (numberp point))
              (- point (akn/count-chars ?\s (substring string 0 point)) (akn/count-chars ?\~ (substring string 0 point)))
            point))))

;; https://github.com/jojojames/fussy?tab=readme-ov-file#corfu-integration
(after! (:and fussy corfu)
  (advice-add #'corfu--capf-wrapper :before #'fussy-wipe-cache))
(add-hook 'corfu-mode-hook #'akn/corfu-fussy-h)
(defun akn/corfu-fussy-h ()
  (setq-local fussy-max-candidate-limit 5000
              fussy-default-regex-fn 'fussy-pattern-first-letter
              fussy-prefer-prefix nil))

;; https://github.com/jojojames/fussy?tab=readme-ov-file#company-integration
(when (modulep! :completion company)
  (after! (:and fussy company)
    (defun j-company-capf (f &rest args)
      "Manage `completion-styles'."
      (if (length= company-prefix 0)
          ;; Don't use `company' for 0 length prefixes.
          (let ((completion-styles (remq 'fussy completion-styles)))
            (apply f args))
        (let ((fussy-max-candidate-limit 5000)
              (fussy-default-regex-fn 'fussy-pattern-first-letter)
              (fussy-prefer-prefix nil))
          (apply f args))))
    (defun j-company-transformers (f &rest args)
      "Manage `company-transformers'."
      (if (length= company-prefix 0)
          ;; Don't use `company' for 0 length prefixes.
          (apply f args)
        (let ((company-transformers '(fussy-company-sort-by-completion-score)))
          (apply f args))))
    (advice-add #'company-auto-begin :before #'fussy-wipe-cache)
    (advice-add #'company--transform-candidates :around #'j-company-transformers)
    (advice-add #'company-capf :around #'j-company-capf)))

;; (akn/incrementally! ())
(custom-set-faces!
  '(completions-first-difference :underline t))
