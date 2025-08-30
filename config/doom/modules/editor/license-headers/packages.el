;; -*- no-byte-compile: t; -*-
;;; editor/license-headers/packages.el

(package! headlice :pin "1d56771651a608aeb2093c267a7d94511b25caff"
  :recipe (:type git
           :repo "https://code.tecosaur.net/tec/headlice"
           :files ("*.el" "licenses" "headers")))

;; also see:
;; https://github.com/condy0919/spdx.el
;; https://github.com/condy0919/spdx.el?tab=readme-ov-file#other-projects
