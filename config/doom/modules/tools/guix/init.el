;;; tools/guix/init.el -*- lexical-binding: t; -*-

(dolist (guix-profile '("/var/guix/profiles/system"
                        "~/.config/guix/current"
                        "~/.guix-profile"))
  (let ((guix-bin (expand-file-name "bin" guix-profile)))
    (push guix-bin exec-path)
    (setenv "PATH" (concat guix-bin ":" (getenv "PATH")))))
