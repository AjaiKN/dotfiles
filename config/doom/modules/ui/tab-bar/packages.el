;; -*- no-byte-compile: t; -*-
;;; ui/tab-bar/packages.el

(package! tab-bar :type 'built-in)
(if (modulep! +bufferlo)
    (package! bufferlo :pin "ef7419b5badc46820fcc6dc73740a00a4740a9ba"
      :recipe (:host github :repo "florommel/bufferlo"))
  (package! tabspaces :pin "f552823f51f11d66492f754deb51abd709c08ed9"))
(package! activities :pin "a4ce6ce854807fe284da41f6d5292edb26c7f983"
  :recipe (:host github :repo "alphapapa/activities.el"))
