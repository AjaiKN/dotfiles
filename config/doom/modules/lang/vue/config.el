;;; lang/vue/config.el -*- lexical-binding: t; -*-

(use-package! vue-ts-mode
  :when (modulep! +tree-sitter)
  :mode ("\\.vue\\'" . vue-ts-mode)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(vue-ts-mode . "vue")))
  (set-eglot-client! 'vue-ts-mode '("vue"))
  (add-hook! 'vue-ts-mode-hook #'lsp!))

(use-package! web-mode
  :when (modulep! -tree-sitter)
  :mode ("\\.vue\\'" . +vue-web-mode)
  :config
  (define-derived-mode +vue-web-mode web-mode "Vue"
    "Major mode for Vue 3 Single File Components.")
  (setq-hook! '+vue-web-mode-hook
    web-mode-content-type "vue"
    web-mode-part-padding 0
    web-mode-script-padding 0
    web-mode-code-indent-offset 0
    web-mode-style-padding 0
    web-mode-css-indent-offset 0
    web-mode-markup-indent-offset tab-width)

  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(+vue-web-mode . "vue")))
  (set-eglot-client! '+vue-web-mode '("vue"))
  (add-hook! '+vue-web-mode-hook #'lsp!))
