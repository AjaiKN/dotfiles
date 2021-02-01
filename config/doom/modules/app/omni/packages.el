;; -*- no-byte-compile: t; -*-
;;; app/omni/packages.el

(package! consult-gh              :pin "acc0e7716bf7f7cbc61db3a3687eb1eed394769e" :disable nil                                      :recipe (:host github :repo "armindarvish/consult-gh"))
(package! consult-omni            :pin "87b5bcf0e55c01e6a4a24ae74ce691f55d1455a2" :disable nil                                      :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
(package! browser-hist            :pin "0372c6d984ca194d9454b14eba6eadec480ec3ff" :disable nil                                      :recipe (:host github :repo "agzam/browser-hist.el"))
