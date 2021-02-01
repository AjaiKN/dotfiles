;;; editor/license-headers/autoload.el -*- lexical-binding: t; -*-

(require 'akn)

(setq! headlice-use-spdx-headers t)

(defun +license-headers/create-license ()
  "Like `headlice-create-license', but always prompt for directory."
  (interactive)
  (akn/letf! ((define-advice headlice--project-root (:filter-return (dirname))
                (read-directory-name "Directory to put LICENSE in: " dirname)))
    (headlice-create-license)))

(defun +license-headers/insert ()
  "Like `headlice-auto-insert', but always insert a license header
(if there's a license in the project)."
  (interactive)
  (headlice-auto-insert '(4)))
(defun +license-headers/insert-here ()
  "Like `headlice-auto-insert', but always insert a license header
at point (if there's a license in the project)."
  (interactive)
  (headlice-auto-insert '(16)))
