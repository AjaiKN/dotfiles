;; -*- no-byte-compile: t; -*-
;;; emacs/tramp-extra/packages.el

(when (modulep! +hlo)
  (package! tramp-hlo :pin "b726b4042e96ac5cead396c8d12c01e6bad2bd78"
    :recipe (:host github :repo "jsadusk/tramp-hlo")))

(when (modulep! +rpc)
  (package! tramp-rpc :pin "5126670acf97abe0367857c642455a984efe019c"
    :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc"
             :files (:defaults "**/*")
             ;; TODO: remove this if tramp-rpc's autoloads are fixed (also from ./config.el)
             :build (:not autoloads))))
