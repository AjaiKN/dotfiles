;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; https://github.com/LemonBreezes/cyber-angel-emacs

(defvar akn/exwm-enabled-p (and (eq 'x (framep (selected-frame)))
                                (not (or (getenv "INSIDE_EXWM")
                                         (getenv "RATPOISON")
                                         (getenv "I3SOCK")
                                         (getenv "KDE_FULL_SESSION")
                                         (getenv "GNOME_DESKTOP_SESSION_ID")
                                         (getenv "XDG_CURRENT_DESKTOP")
                                         (getenv "WAYLAND_DISPLAY")))
                                ;; https://emacs.stackexchange.com/a/60455
                                (and (executable-find "wmctrl")
                                     (eq window-system 'x)
                                     (progn
                                       (shell-command "wmctrl -m ; echo $?" " *window-manager*" " *window-manager-error*")
                                       ;; if there was an error detecting the window manager, initialize EXWM
                                       (get-buffer " *window-manager-error*"))))
  "Whether EXWM is enabled.")

(when akn/exwm-enabled-p
  (akn/load! "start"))
