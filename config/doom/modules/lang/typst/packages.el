;; -*- no-byte-compile: t; -*-
;;; lang/typst/packages.el

(package! typst-ts-mode :pin "972dc69d6b8a3f8983f6b8000654f59c8a8d05ba"
  :recipe (:type git :host codeberg :repo "meow_king/typst-ts-mode"))

(package! typst-preview :pin "69918d5f5b77032d8a5f796793b8030b4f19f5d6"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
