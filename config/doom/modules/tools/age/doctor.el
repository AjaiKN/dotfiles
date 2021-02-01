;;; tools/age/doctor.el -*- lexical-binding: t; -*-

(unless (or (executable-find "age") (executable-find "rage"))
  (warn! "Couldn't find age or rage executable"))
