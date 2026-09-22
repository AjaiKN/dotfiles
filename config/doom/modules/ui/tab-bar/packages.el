;;; ui/tab-bar/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! tab-bar :type 'built-in)
(if (modulep! +bufferlo)
    (package! bufferlo :pin "1ab597c021ee33511fdaad942cc6dd5ac064f6ba"
      :recipe (:host github :repo "florommel/bufferlo"))
  (package! tabspaces :pin "2bfb7361b8d82f660eca8bd2e131b5ca53d56916"))
(package! activities :pin "5025962126d140a7e26d36c3a2750bf4ff0bfd45"
  :recipe (:host github :repo "alphapapa/activities.el"))
