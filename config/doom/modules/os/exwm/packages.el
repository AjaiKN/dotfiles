;; -*- no-byte-compile: t; -*-
;;; os/exwm/packages.el

;; https://github.com/LemonBreezes/cyber-angel-emacs

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib") :pin "de3d8379b631eef57e683af0dec65c8d4fb0350f")

(package! xelb :recipe
  (:host github :repo "emacs-exwm/xelb") :pin "6dfda93a39b6cc1919779f71dc96ef353061aa3e")
(package! exwm :recipe
  (:host github :repo "emacs-exwm/exwm") :pin "1d68e170d58176d931200576e0322c57fa2326df")
(package! exwm-mff :pin "89206f2e3189f589c27c56bd2b6203e906ee7100")
(when (modulep! :editor evil +everywhere)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil") :pin "722947890abaa3483f71aa5f9082897e939c0eb9")
  (package! exwm-firefox-evil :pin "c87d601de9bad4d3cbf41c69281073b465a66769")
  (package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher") :pin "d5015e394b0a666a8c7c4d4bdf786266e773b145"))
