;; -*- no-byte-compile: t; -*-
;;; lang/vue/packages.el

(if (modulep! +tree-sitter)
    (package! vue-ts-mode :pin "efc7031f50bbfd2a3293aee4fcb34bf0503b7f83"
      :recipe (:host github :repo "8uff3r/vue-ts-mode"))
  ;; not pinning because :lang web pins it
  (package! web-mode))
