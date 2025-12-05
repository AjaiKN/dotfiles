;;; term/eshell-extra/config.el -*- lexical-binding: t; -*-

;;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#last-results (CC0)
(add-hook 'eshell-post-command-hook #'+eshell--store-last-output-h)
(after! esh-var
  (pushnew! eshell-variable-aliases-list
            (list "$"      #'+eshell-output-text)
            (list "_"      #'+eshell-output-list)
            (list "OUTPUT" #'+eshell-output-file)))
