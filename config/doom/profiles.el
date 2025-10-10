;; profiles.el - Doom Emacs profile definitions -*- mode: lisp-data; -*-

;; Defines multiple Emacs configurations that can be switched between
;; using Doom's profile system.

((default) ; my regular Doom profile
 (basic (user-emacs-directory . "~/.config/emacs.basic/"))
 (prelude (user-emacs-directory . "~/.config/emacs.prelude/"))
 (spacemacs (user-emacs-directory . "~/.config/emacs.spacemacs/"))
 (minimal (user-emacs-directory . "~/.config/emacs.minimal/") (minimal-emacs-user-directory . "~/.config/emacs.minimal.config/")))
