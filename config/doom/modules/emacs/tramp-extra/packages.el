;; -*- no-byte-compile: t; -*-
;;; emacs/tramp-extra/packages.el

(when (modulep! +hlo)
  (package! tramp-hlo :pin "b726b4042e96ac5cead396c8d12c01e6bad2bd78"
    :recipe (:host github :repo "jsadusk/tramp-hlo")))

(when (modulep! +rpc)
  (package! tramp-rpc :pin "e0d0c4984e4170dc599b87858b68eacf34838e09"
    :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc"
             :files (:defaults "**/*")
             ;; TODO: remove this if tramp-rpc's autoloads are fixed (also from ./config.el)
             :build (:not autoloads))))
