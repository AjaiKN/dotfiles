;;; tools/mason/autoload.el -*- lexical-binding: t; -*-

(defcustom +mason-lsp-programs
  '((ada "ada-language-server")
    (beancount "beancount-language-server")
    (cc "clangd" "cmake-language-server")
    (clojure "clojure-lsp")
    (csharp "omnisharp")
    (elixir "elixir-ls")
    (elm "elm-language-server")
    (fortran "fortls")
    (go "gopls")
    (graphql "graphql-language-service-cli")
    ;; (haskell "haskell-language-server")
    (java "jdtls")
    (javascript "typescript-language-server")
    (json "json-lsp")
    (julia "julia-lsp")
    (kotlin "kotlin-language-server")
    (latex "digestif" "texlab")
    (lua "emmylua_ls" "lua-language-server")
    (nix "nil" "rnix-lsp")
    (ocaml "ocaml-lsp")
    (php "intelephense")
    (purescript "purescript-language-server")
    (python "pyright" "basedpyright")
    (qt "qmlls")
    (ruby "ruby-lsp" "solargraph")
    (rust "rust-analyzer")
    (sh "bash-language-server")
    (sml "millet")
    (web "html-lsp" "css-lsp" "lemminx")
    (yaml "yaml-language-server")
    (zig "zls")

    ;; (jsonian "json-lsp")
    ;; (obsidian "markdown-oxide")
    ;; (vimrc "vim-language-server")
    (roc "roc_language_server")
    (typst "tinymist"))
  "A mapping of languages to LSP packages.

This is a list of lists of the form (LANG PKG...), where LANG is the
name of a Doom language module (e.g., :lang clojure), and each PKG is a
Mason package that will be installed if the language model is enabled."
  :group '+mason
  :type '(alist :key-type symbol :value-type (repeat string)))

(defvar +mason--ensured nil
  "Non-nil if mason has already been ensured.")

(defvar +mason--packages-pending nil
  "Packages that we've asked to install but haven't finished installing.")
(defvar +mason--packages-failed nil
  "Packages that failed to install.")

;;;###autodef
(defmacro mason-after-ensured! (&rest body)
  "Run BODY after Mason has been ensured."
  (declare (indent 0))
  `(if +mason--ensured
       ,(macroexp-progn body)
     (require 'mason)
     (mason-ensure
      (lambda ()
        (setq +mason--ensured t)
        ,@body))))

(defun +mason--print-progress ()
  "Print a temporary message with the list of packages that are currently
being installed."
  (when +mason--packages-pending
    (princ "\r")
    (print! (item "Installing %d Mason packages: %s%s")
            (length +mason--packages-pending)
            (truncate (string-trim (format "%s" +mason--packages-pending) "(" ")"))
            (if (or init-file-debug (not noninteractive)) "" "\e[1A"))))

;;;###autodef
(defun mason-install! (package &optional suffix)
  (push package +mason--packages-pending)
  (mason-after-ensured!
    (with-demoted-errors "Mason error: %S"
      (mason-install package
                     'force nil
                     (lambda (success)
                       (if success
                           (print! (success "\rInstalled %s%s") package (or suffix ""))
                         (print! (error "\rFailed to install %s%s!") package (or suffix ""))
                         (push package +mason--packages-failed))
                       (when (not success)
                         (warn "Couldn't install %s%s" package (or suffix "")))
                       (setq +mason--packages-pending (delete package +mason--packages-pending))
                       (+mason--print-progress))))))

;;;###autoload
(defun +mason/install-all-lsps (&optional reinstall?)
  (interactive)
  (when (modulep! :tools lsp)
    (mason-after-ensured!
      (pcase-dolist (`(,language . ,packages) +mason-lsp-programs)
        (let ((suffix (format " (:lang %s)" language)))
          (dolist (package packages)
            (if (and (not reinstall?)
                     (file-exists-p! (file-name-concat mason-dir "packages" package))
                     (not (directory-empty-p (file-name-concat mason-dir "packages" package))))
                (print! (success "%s already installed%s") package suffix)
              (when (eval `(modulep! :lang ,language +lsp) t)
                (mason-install! package suffix))))))
      (+mason--print-progress))))
