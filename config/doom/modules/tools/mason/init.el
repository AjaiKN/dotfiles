;;; tools/mason/init.el -*- lexical-binding: t; -*-

(message "init")

(defvar +mason--ensured nil)

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
    (haskell "haskell-language-server")
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
  ""
  :group '+mason
  :type '(alist :key-type symbol :value-type (repeat string)))

;; (after! mason
;;   (mason-ensure
;;    (lambda ()
;;      (message "mason ensured!")
;;      (setq +mason--ensured t))))

;; (add-hook! 'doom-before-sync-hook
;;   (defun +mason--install-h3 ()
;;     (message "hihi")))

;; (add-hook! 'doom-after-sync-hook
;;   (defun +mason--install-h2 ()
;;     (message "hi")
;;     (load! "./autoload.el")
;;     (message "Installing LSPs...")
;;     (+mason/install-all-lsps)
;;     (message "Installing LSPs...")))

(message "init done")
