;; -*- no-byte-compile: t; -*-
;;; ui/tab-bar/packages.el

(package! tab-bar :type 'built-in)
(if (modulep! +bufferlo)
    (package! bufferlo :pin "a4971b52232ff622f768fcd569c3ae5929580f3f"
      :recipe (:host github :repo "florommel/bufferlo"))
  (package! tabspaces :pin "cd9f780bd69747fb8a598be1d4347e2e161c2d82"))
(package! activities :pin "5025962126d140a7e26d36c3a2750bf4ff0bfd45"
  :recipe (:host github :repo "alphapapa/activities.el"))
