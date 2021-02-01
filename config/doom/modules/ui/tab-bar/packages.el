;; -*- no-byte-compile: t; -*-
;;; ui/tab-bar/packages.el

(package! tab-bar :type 'built-in)
(if (modulep! +bufferlo)
    (package! bufferlo :pin "2c3c5ec3d3eec20efbf23b08ecf78c9d174bdcd8"
      :recipe (:host github :repo "florommel/bufferlo"))
  (package! tabspaces :pin "f552823f51f11d66492f754deb51abd709c08ed9"))
(package! activities :pin "d735c0f2c714ac98248ee17d765bfa8310201d53"
  :recipe (:host github :repo "alphapapa/activities.el"))
