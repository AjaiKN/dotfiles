;; -*- no-byte-compile: t; -*-
;;; app/omni/packages.el

(package! consult-gh              :pin "f76011cafdfb5ab3cff5c93865e4db21713d67cb" :disable nil                                      :recipe (:host github :repo "armindarvish/consult-gh"))
(package! consult-omni            :pin "d0a24058bf0dda823e5f1efcae5da7dc0efe6bda" :disable nil                                      :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
(package! browser-hist            :pin "1cd80081feaab99fef9e8eadd55d68b3cef90144" :disable nil                                      :recipe (:host github :repo "agzam/browser-hist.el"))
