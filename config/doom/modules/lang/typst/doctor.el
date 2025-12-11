;;; lang/typst/doctor.el -*- lexical-binding: t; -*-

(when (not (executable-find "typst"))
  (warn! "Typst isn't installed."))

(when (and (modulep! +lsp)
           (not (executable-find "tinymist")))
  (warn! "Couldn't find tinymist language server."))
