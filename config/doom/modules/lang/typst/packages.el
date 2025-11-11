;; -*- no-byte-compile: t; -*-
;;; lang/typst/packages.el

(package! typst-ts-mode :pin "7c2ef0d5bd2b5a8727fe6d00938c47ba562e0c94"
  :recipe (:type git :host codeberg :repo "meow_king/typst-ts-mode"))

(package! typst-preview :pin "c685857f2d61133fc268509b39796b2718728f29"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
