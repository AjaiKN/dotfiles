;;; lang/vue/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(if (modulep! +tree-sitter)
    (package! vue-ts-mode :pin "df0a7e03660840ec53ab746b9f6acad9275bbf8c"
      :recipe (:host github :repo "8uff3r/vue-ts-mode"))
  ;; not pinning because :lang web pins it
  (package! web-mode))
