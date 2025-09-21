;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; https://github.com/LemonBreezes/cyber-angel-emacs

(defvar akn/exwm-enabled-p (and (featurep :system 'linux)
                                (eq 'x (framep (selected-frame)))
                                (eq 'x window-system)
                                (not (or (getenv "INSIDE_EXWM")
                                         (getenv "RATPOISON")
                                         (getenv "I3SOCK")
                                         (getenv "KDE_FULL_SESSION")
                                         (getenv "GNOME_DESKTOP_SESSION_ID")
                                         (getenv "XDG_CURRENT_DESKTOP")
                                         (getenv "WAYLAND_DISPLAY")))
                                ;; https://emacs.stackexchange.com/a/60455
                                (if (executable-find "wmctrl")
                                    (progn
                                      (shell-command "wmctrl -m ; echo $?" " *window-manager*" " *window-manager-error*")
                                      ;; if there was an error detecting the window manager, initialize EXWM
                                      (get-buffer " *window-manager-error*"))
                                  t))
  "Whether EXWM is enabled.")

(use-package! exwm
  :when akn/exwm-enabled-p
  :demand t
  :config
  (akn/load! "start"))
