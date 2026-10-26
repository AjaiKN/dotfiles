;;; os/exwm/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; https://github.com/LemonBreezes/cyber-angel-emacs

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib") :pin "2cac10a793cd913f08b1fbfe8f3609bff9e5f8fe")

(package! xelb :recipe
  (:host github :repo "emacs-exwm/xelb") :pin "638692fac2e3ef081cd8c39891d6d6e15032ff17")
(package! exwm :recipe
  (:host github :repo "emacs-exwm/exwm") :pin "5cd76bb639df30a8a7d0f0bbf2edeacd75be0548")
(package! exwm-mff :pin "89206f2e3189f589c27c56bd2b6203e906ee7100")
(when (modulep! :editor evil +everywhere)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil") :pin "722947890abaa3483f71aa5f9082897e939c0eb9")
  (package! exwm-firefox-evil :pin "c87d601de9bad4d3cbf41c69281073b465a66769")
  (package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher") :pin "5493b8ec39c00b4971d260c03e8bbe93b25bb697"))
