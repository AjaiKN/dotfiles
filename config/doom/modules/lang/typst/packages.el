;;; lang/typst/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! typst-ts-mode :pin "ee17b331ebfa8d8b7c2f54a1c39ab3d76e134706"
  :recipe (:type git :host codeberg :repo "meow_king/typst-ts-mode"
           ;; TODO: PR to fix: Symbol's function definition is void: define-compilation-mode
           :build (:not autoloads)))

(package! typst-preview :pin "f2903a1b98e13be7c927de835ae0d9159dd9fb9a"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
