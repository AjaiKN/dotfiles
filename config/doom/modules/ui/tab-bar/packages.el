;; -*- no-byte-compile: t; -*-
;;; ui/tab-bar/packages.el

(package! tab-bar :type 'built-in)
(if (modulep! +bufferlo)
    (package! bufferlo :pin "ee7db7237b152a06b01375bbf57b2965a354ba9f"
      :recipe (:host github :repo "florommel/bufferlo"))
  (package! tabspaces :pin "cd9f780bd69747fb8a598be1d4347e2e161c2d82"))
(package! activities :pin "5025962126d140a7e26d36c3a2750bf4ff0bfd45"
  :recipe (:host github :repo "alphapapa/activities.el"))
