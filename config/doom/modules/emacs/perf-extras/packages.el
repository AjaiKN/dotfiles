;;; emacs/perf-extras/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! benchmark-init          :pin "54b9703389f25012e4cc20fe4a0d4ea253ce4820" :disable nil)
(package! explain-pause-mode      :pin "ac3eb69f36f345506aad05a6d9bc3ef80d26914b" :disable nil                                      :recipe (:host github :repo "lastquestion/explain-pause-mode"))
