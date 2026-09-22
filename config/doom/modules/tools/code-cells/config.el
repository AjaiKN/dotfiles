;;; config/doom/modules/tools/code-cells/config.el -*- lexical-binding: t; -*-

(use-package! code-cells
  :ghook ('python-base-mode-hook #'code-cells-mode-maybe))
