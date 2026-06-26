;;; lang/mediawiki/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! mediawiki ;:pin "8002f2fea253b5c2f601325c5b4968e568a3aa2a"
  :recipe (:local-repo "~/prog/emacs/mediawiki-el"))

;; (when (modulep! +wikipedia)
;;   (package! wikipedia ;:pin "d7219cd453a93b93598a339d20927bdf60eded8d"
;;     :recipe (:host github :repo "benthamite/wikipedia"
;;              :local-repo "~/prog/emacs/wikipedia")))
