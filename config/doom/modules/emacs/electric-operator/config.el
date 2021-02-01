;;; emacs/electric-operator/config.el -*- lexical-binding: t; -*-

(require 'akn)

(defvar-local electric-operator-mode nil)

(defun akn/do-enable-electric-operator-mode ()
  (when (and akn/electric-operator-global-mode ;make sure it hasn't been turned off again since the hook was added
             (doom-real-buffer-p (akn/this-buffer))
             (progn (require 'easy-mmode) ;make sure this is still one of the modes where it should be enabled
                    (easy-mmode--globalized-predicate-p akn/electric-operator-global-modes)))
    (electric-operator-mode)))
(defun akn/enable-electric-operator-mode ()
  (if (buffer-modified-p)
      (akn/do-enable-electric-operator-mode)
    (add-hook 'first-change-hook #'akn/do-enable-electric-operator-mode nil t)))
(define-globalized-minor-mode akn/electric-operator-global-mode electric-operator-mode akn/enable-electric-operator-mode
  ;; :predicate `((not ,@akn/lisp-like-modes dired-mode sh-mode) prog-mode) ;text-mode)
  :predicate `(f90-mode latex-math swift-mode julia-mode scss-mode css-mode sql-mode coffee-mode php-mode java-mode ruby-mode cperl-mode perl-mode inferior-ess-r-mode ess-r-mode ess-mode rust-mode typescript-mode js2-mode js-mode inferior-python-mode python-mode arduino-mode c++-mode c-mode)
  :group 'akn)

(add-hook 'doom-first-buffer-hook #'akn/electric-operator-global-mode)

;; https://github.com/noctuid/dotfiles/blob/master/emacs/.config/emacs/awaken.org#electric-operator-mode
(use-package! electric-operator
  :defer t
  ;; ;; officially supported: see (progn (require 'electric-operator) (hash-table-keys electric-operator--mode-rules-table))
  ;; :hook ((f90-mode latex-math swift-mode julia-mode scss-mode css-mode sql-mode coffee-mode php-mode java-mode ruby-mode cperl-mode perl-mode inferior-ess-r-mode ess-r-mode ess-mode rust-mode typescript-mode js2-mode js-mode inferior-python-mode python-mode arduino-mode c++-mode c-mode text-mode) . electric-operator-mode)
  ;; ;; other stuff
  ;; :hook ((python-base-mode inferior-python-mode ess-r-mode inferior-ess-r-mode rjsx-mode typescript-mode web-mode nodejs-repl-mode nodejs-repl js-base-mode typescript-ts-base-mode js2-mode coffee-mode coffee-ts-mode c-mode cpp-mode c-ts-base-mode java-mode java-ts-mode rustic-mode rust-mode rustic-ts-mode rust-ts-mode php-mode php-ts-mode julia-mode ess-julia-mode inferior-ess-julia-mode julia-repl-mode css-mode css-ts-mode fortran-mode fortran-ts-mode perl-mode cperl-mode ruby-base-mode inf-ruby-mode LaTeX-mode roc-mode haskell-mode elm-mode elm-ts-mode haskell-ts-mode zig-mode sml-mode) . electric-operator-mode))
  :init
  (akn/doom-prioritize-load-packages-incrementally
   '(cl-lib regexp-opt cc-defs cc-vars cc-engine cc-styles cc-cmds cc-align cc-menus cc-guess easymenu cc-fonts cc-mode
     thingatpt
     macroexp gv cl-macs
     dash
     electric-operator)))

