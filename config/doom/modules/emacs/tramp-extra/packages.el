;;; emacs/tramp-extra/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(when (modulep! +hlo)
  (package! tramp-hlo :pin "b726b4042e96ac5cead396c8d12c01e6bad2bd78"
    :recipe (:host github :repo "jsadusk/tramp-hlo")))

(when (modulep! +rpc)
  (package! msgpack :pin "5353a7b2da854c843cbec4536996242001f63471")
  (package! tramp-rpc :pin "c00e7171cb390a8bf53ec818ea95167a0d8b7945"
    :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc"
             :files (:defaults "**/*"))))
