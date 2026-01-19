;;; tools/mise/config.el -*- lexical-binding: t; -*-

(use-package! mise
  :when (executable-find "mise")
  :ghook ('doom-first-file-hook #'global-mise-mode)
  :config
  ;; (setq! mise-debug t)
  (akn/run-command `("mise" "trust" ,(file-truename "~/.config/mise/config.toml"))
                   :boring t))

;; If the user saves a mise config file, we can trust it.
(def-project-mode! +mise-trust-mode
  :match (rx (seq "/"
                  (or (seq (? ".")
                           (or "mise.local.toml"
                               "mise.toml"
                               "mise/config.toml"
                               (seq "mise/conf.d/" (* (not "/")) ".toml")))
                      ".tool-versions"
                      (seq "." (* (not "/")) "-version")
                      "main.tf" ".sdkmanrc" ".nvmrc" ".yvmrc")
                  eos))
  :on-enter (add-hook! 'after-save-hook :local
              (defun +mise-trust-h ()
                "Whenever the user saves this file, run mise trust on it."
                (akn/run-command `("mise" "trust" ,(expand-file-name buffer-file-truename))
                                 ;; :output-buffer " *mise-trust*"
                                 :boring t))))
