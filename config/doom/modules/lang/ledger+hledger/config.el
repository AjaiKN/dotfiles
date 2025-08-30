;;; lang/ledger+hledger/config.el -*- lexical-binding: t; -*-

;; https://hledger.org/editors.html#emacs

(use-package! ledger-mode
  :mode ("\\.hledger\\'" "\\.ledger\\'")
  :config
  (setq!
   ledger-binary-path (expand-file-name "hledger.sh" (dir!)) ;"hledger"
   ledger-default-date-string "%Y-%m-%d"

   ledger-mode-should-check-version nil
   ledger-report-auto-width nil
   ledger-report-links-in-register nil
   ledger-report-native-highlighting-arguments '("--color=always")))
