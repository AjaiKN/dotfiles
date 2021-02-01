;;; os/emacs-plus/init.el -*- lexical-binding: t; -*-

;; seems to freeze on mac Emacs Plus for some reason
(advice-add #'doom-compile-functions :override #'ignore)
