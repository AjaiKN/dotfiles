;;; os/emacs-mac/config.el -*- lexical-binding: t; -*-

(require 'akn)

;;;; Fullscreen
(when (modulep! +fullscreen)
  ;; doesn't do anything for emacs-mac port
  (setq ns-use-native-fullscreen t)

  ;; If this ever stops working, run "sudo tccutil reset Accessibility org.gnu.Emacs".
  ;; Then, go to System Settings > Privacy & Security > Accessibility > enable/add Emacs

  (defun akn/fullscreenp ()
    (frame-parameter nil 'fullscreen))
  (defun akn/fullscreen-on ()
    (interactive)
    (when (display-graphic-p)
      (when akn/has-fullscreened
        (setq akn/should-become-transparent-when-leaving-fullscreen (akn/transparent-p)))
      (akn/run-command (format "osascript -e 'tell application \"System Events\" to tell (first process whose unix id is %d) to set value of attribute \"AXFullScreen\" of window 1 to true'"
                              (emacs-pid))
                      :name "applescript-fullscreen"
                      :boring t
                      :on-finish
                      (lambda (fail)
                        (if fail
                            (progn
                              (kill-new "sudo tccutil reset Accessibility org.gnu.Emacs")
                              (message (concat "Fullscreening failed. \n"
                                               "Try running `sudo tccutil reset Accessibility org.gnu.Emacs' (copied to clipboard). \n"
                                               "Then, go to System Settings > Privacy & Security > Accessibility > enable/add Emacs.")))
                          (setq akn/has-fullscreened t)
                          (akn/transparency-off))))))
  (defun akn/fullscreen-off ()
    (interactive)
    (when (display-graphic-p)
      (set-frame-parameter nil 'fullscreen nil)
      (when akn/should-become-transparent-when-leaving-fullscreen
        (akn/transparency-on))))
  (defcustom akn/should-become-transparent-when-leaving-fullscreen t
    "Should the frame become partially transparent when leaving fullscreen mode?"
    :type '(boolean)
    :group 'akn)
  (defun akn/fullscreen-toggle ()
    (interactive)
    (if (akn/fullscreenp)
        (akn/fullscreen-off)
      (akn/fullscreen-on)))

  ;; fullscreen on startup
  (defvar akn/has-fullscreened nil)
  ;; (when (and (not (akn/fullscreenp))
  ;;            (not akn/has-fullscreened))
  ;;   (akn/fullscreen-on))
  (add-hook 'window-setup-hook #'akn/fullscreen-on))

;;;; Misc
(setq mac-pass-control-to-system nil
      mac-pass-command-to-system nil

      ;; This is specific to emacs-mac (https://bitbucket.org/mituharu/emacs-mac).
      ;; Ideally I would use the builtin emacs pixel-scroll-precision-mode, but that
      ;; doesn't seem to be working.
      ;; Doom Emacs disables this by default b/c it's kind of laggy.
      mac-mouse-wheel-smooth-scroll t
      ;; Not sure why Doom changed this to t, which seems to make it more laggy (but I can't tell for 100% sure)?
      ;; I don't really know what this actually does.
      mac-redisplay-dont-reset-vscroll nil

      ;; mac-right-command-modifier 'hyper
      mac-function-modifier 'hyper

      mac-system-move-file-to-trash-use-finder t)

(map! :map akn/local-smooth-scroll-disabled-mode-map
      ;; instead of #'mwheel-scroll
      [remap pixel-scroll-precision] #'mac-mwheel-scroll)

;;;; context menu bug
(after! mouse
  ;; In this emacs-mac port, the Mac system context menu (right click) is broken
  ;; and crashes emacs if you hover over a system menu item.
  ;; So we're just using the regular context-menu-entry instead.
  (define-key context-menu-mode-map [down-mouse-3]
              (if (featurep 'mac)
                  ;; CHANGED from `#'mac-mouse-context-menu' to `context-menu-entry'
                  context-menu-entry
                context-menu-entry))
  ;; same for control-click
  (define-key context-menu-mode-map [C-down-mouse-1]
              (if (featurep 'mac)
                  ;; CHANGED from `#'mac-mouse-context-menu' to `context-menu-entry'
                  context-menu-entry
                context-menu-entry)))
