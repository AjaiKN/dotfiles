;;; tools/vim-file-locals/config.el -*- lexical-binding: t; -*-

(use-package! vim-file-locals
  :ghook 'doom-first-buffer-hook
  :config
  (setq! vim-file-locals-verbose t
         ;; By default, vim looks at the first and last 5 lines, so no point in
         ;; looking at more than that.
         vim-file-locals-modelines 5
         vim-file-locals-prefer-auto-mode nil)

  (add-hook 'vim-file-locals-after-apply-hook
            #'+vim-file-locals--tab-width-stuff-h))
