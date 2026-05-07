;; -*- no-byte-compile: t; -*-
;;; emacs/tramp-extra/packages.el

(when (modulep! +hlo)
  (package! tramp-hlo :pin "b726b4042e96ac5cead396c8d12c01e6bad2bd78"
    :recipe (:host github :repo "jsadusk/tramp-hlo")))

(when (modulep! +rpc)
  (package! msgpack :pin "90e3086f259549b1667a3c5b9aa2d70aaeaa4d3d")
  (package! tramp-rpc :pin "26d82116d4d94d4452bada990aacf5d54ff10a82"
    :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc"
             :files (:defaults "**/*"))))
