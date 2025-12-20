;; -*- no-byte-compile: t; -*-
;;; ui/tab-bar/packages.el

(package! tab-bar :type 'built-in)
(if (modulep! +bufferlo)
    (package! bufferlo :pin "885a4f29ba5cea47e8a6206833d220c70e9b697b"
      :recipe (:host github :repo "florommel/bufferlo"))
  (package! tabspaces :pin "051d8cecc67a83531c3c3e68e13f2cb297a4cfe2"))
(package! activities :pin "5025962126d140a7e26d36c3a2750bf4ff0bfd45"
  :recipe (:host github :repo "alphapapa/activities.el"))
