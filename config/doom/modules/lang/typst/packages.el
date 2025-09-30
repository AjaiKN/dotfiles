;; -*- no-byte-compile: t; -*-
;;; lang/typst/packages.el

(package! typst-ts-mode :pin "fa03e477dfc57ea498ef9a624b2ec7bf655dc25c"
  :recipe (:type git :host codeberg :repo "meow_king/typst-ts-mode"))

(package! typst-preview :pin "4f3decb8d9b85b1a06b0dfbd0fccaa0d2d2c24b4"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
