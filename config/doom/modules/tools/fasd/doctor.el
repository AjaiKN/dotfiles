;;; tools/fasd/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "fasd")
  (warn! "Couldn't find fasd executable"))
