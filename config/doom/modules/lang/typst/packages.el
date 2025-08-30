;; -*- no-byte-compile: t; -*-
;;; lang/typst/packages.el

(package! typst-ts-mode :pin "99fd3d4d889c0c7365563905d1fa0e08b7e5082b"
  :recipe (:type git :host codeberg :repo "meow_king/typst-ts-mode"))

(package! typst-preview :pin "4f3decb8d9b85b1a06b0dfbd0fccaa0d2d2c24b4"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
