;;; editor/tempel/config.el -*- lexical-binding: t; -*-

(use-package! tempel
  :defer t
  :ghook ('(prog-mode-hook text-mode-hook conf-mode-hook) #'+tempel-setup-capf-h)
  :ghook ('doom-first-buffer-hook #'global-tempel-abbrev-mode)
  :config
  (setq! tempel-path (list (expand-file-name "snippets.eld" doom-user-dir)))
  (add-to-list 'tempel-user-elements #'+tempel-add-user-elements))

(use-package! consult-tempel
  :when (modulep! :completion vertico)
  :defer t
  :init (map! [remap tempel-insert] #'consult-tempel))
