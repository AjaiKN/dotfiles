;;; lang/jsonian/config.el -*- lexical-binding: t; -*-

;; TODO: JSON5 (.parcelrc should be moved to this)
;; - https://github.com/dochang/json5-ts-mode (on MELPA)
;; - https://fuchsia.googlesource.com/fuchsia/+/main/scripts/emacs/fuchsia-json5.el

(use-package! jsonian
  :init
  ;; https://github.com/microsoft/vscode/blob/7b1c3d3dcee38406280f67f9909508680da0898d/extensions/json/package.json#L25
  ;; https://github.com/microsoft/vscode/blob/7b1c3d3dcee38406280f67f9909508680da0898d/extensions/configuration-editing/package.json#L40
  (pushnew! auto-mode-alist
            (cons (rx (or (: "." ;extensions
                             (or "json"
                                 "bowerrc"
                                 "webmanifest"
                                 "js.map"
                                 "har"
                                 "jsonld"
                                 "geojson"
                                 ;; "ipynb"
                                 "vuerc"
                                 "code-profile"
                                 (: (or "js" "css" "ts") "map")
                                 (: "js" (or "lint" "cs") (? "rc"))))
                          (: "/" ;filenames
                             (or "composer.lock" ".watchmanconfig" "flake.lock")))
                      eos)
                  #'jsonian-mode)
            (cons (rx (or (: "." ;extensions
                             (or ".jsonc"
                                 (: ".js" (or "hint" "fmt") (? "rc"))
                                 (: ".eslintrc" (? ".json"))
                                 ".swcrc"
                                 ".hintrc"
                                 ".babelrc"
                                 ".parcelrc"
                                 ".code-workspace"))
                          (: (or "language-configuration" "icon-theme" "color-theme") ".json")
                          (: "/"
                             (or (: (or "settings" "launch" "tasks" "mcp" "keybindings" "extensions" "argv" "profiles" "devcontainer" ".devcontainer"
                                        "babel.config" ".babelrc" "typedoc")
                                    ".json")
                                 "/bun.lock"
                                 "/.ember-cli")))
                      eos)
                  #'jsonian-c-mode)
            (cons (rx (or ".jsonl" ".ndjson")
                      eos)
                  #'+jsonian-lines-mode))
  (pushnew! major-mode-remap-defaults
            '(json-mode . jsonian-mode)
            '(json-ts-mode . jsonian-mode)
            '(js-json-mode . jsonian-mode)
            '(jsonc-mode . jsonian-c-mode)
            '(+json-lines-mode . +jsonian-lines-mode))
  :config
  (after! so-long
    ;; (jsonian-no-so-long-mode))
    (setq jsonian--so-long-predicate so-long-predicate)
    (setq so-long-predicate
          (lambda ()
            ;; CHANGED from jsonian-no-so-long-mode: use derived-mode-p instead of checking major-mode directly
            (unless (derived-mode-p 'jsonian-mode)
              (funcall jsonian--so-long-predicate)))))
  (after! flycheck
    (jsonian-enable-flycheck))

  (set-electric! 'jsonian-mode :chars '(?\n ?: ?{ ?}))

  (map! :map jsonian-mode-map
        [remap consult-imenu] #'jsonian-find
        [remap imenu]         #'jsonian-find
        :localleader
        :desc "Toggle comments (jsonc)" "c" (cmds! (derived-mode-p 'jsonian-c-mode) #'jsonian-mode #'jsonian-c-mode)
        :desc "Format region"           "f" #'jsonian-format-region
        :desc "Copy path"               "p" #'jsonian-path
        :desc "Edit string"             "s" #'jsonian-edit-string
        :desc "Up (enclosing item)"     "u" #'jsonian-enclosing-item)

  (defadvice! +jsonian-lines--narrow-to-line-a (fn &rest args)
    :around #'jsonian-path
    ;; :around #'jsonian-find
    ;; :around #'jsonian--cached-find-children
    ;; :around #'jsonian--display-path
    ;; :around #'jsonian--find-children
    ;; :around #'jsonian--find-completion
    ;; :around #'jsonian--path
    ;; :around #'jsonian--reconstruct-path
    ;; :around #'jsonian--snap-to-node
    ;; :around #'jsonian--valid-path
    ;; :around #'jsonian--completing-t
    (if (derived-mode-p '+jsonian-lines-mode)
        (save-restriction
          (narrow-to-region (line-beginning-position) (line-end-position))
          (apply fn args))
      (apply fn args))))
