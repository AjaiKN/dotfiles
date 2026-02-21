;;; editor/tempel/config.el -*- lexical-binding: t; -*-

(use-package! tempel
  :defer t
  :ghook ('(prog-mode-hook text-mode-hook conf-mode-hook) #'+tempel-setup-capf-h)
  :ghook ('doom-first-buffer-hook #'global-tempel-abbrev-mode)
  :config
  (setq! tempel-path (list (expand-file-name "snippets.eld" doom-user-dir)))
  (add-to-list 'tempel-user-elements #'+tempel-add-user-elements)

  (map! :map tempel-map
        :gie "TAB"       #'tempel-next
        :gie "<tab>"     #'tempel-next
        :gie "<backtab>" #'tempel-previous
        :gie "S-<tab>"   #'tempel-previous
        :gie "S-TAB"     #'tempel-previous))

(use-package! consult-tempel
  :when (modulep! :completion vertico)
  :defer t
  :init (map! [remap tempel-insert] #'consult-tempel))
