;;; tools/stgit/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "stg")
  (warn! "Couldn't find stg executable"))
