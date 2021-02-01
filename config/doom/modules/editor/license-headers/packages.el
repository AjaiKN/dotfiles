;; -*- no-byte-compile: t; -*-
;;; editor/license-headers/packages.el

(package! headlice :pin "ad9cdeee417b29ac14b3efd4e211e1ae83f86795"
  :recipe (:type git
           :repo "https://code.tecosaur.net/tec/headlice"
           :files ("*.el" "licenses" "headers")))

;; also see:
;; https://github.com/condy0919/spdx.el
;; https://github.com/condy0919/spdx.el?tab=readme-ov-file#other-projects
