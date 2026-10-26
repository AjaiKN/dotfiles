;;; editor/license-headers/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! headlice :pin "5f17e44f8d71e270df5353483900d2345390be96"
  :recipe (:type git
           :repo "https://code.tecosaur.net/tec/headlice"
           :files ("*.el" "licenses" "headers")))

;; also see:
;; https://github.com/condy0919/spdx.el
;; https://github.com/condy0919/spdx.el?tab=readme-ov-file#other-projects
