;;; lang/roc/doctor.el -*- lexical-binding: t; -*-

(when (not (executable-find "roc"))
  (warn! "Roc isn't installed."))

(when (and (modulep! +lsp)
           (not (executable-find "roc_language_server")))
  (warn! "Couldn't find roc_language_server."))
