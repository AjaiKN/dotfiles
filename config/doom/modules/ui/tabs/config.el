;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(after! (:or tab-line tab-bar)
  (custom-set-faces!
    '((tab-line tab-bar)
      :family "Liberation Sans"
      :foreground unspecified)
    '(tab-line-highlight)
    '(tab-line-close-highlight
      :foreground "red")
    '((tab-line-tab tab-line-tab-current tab-line-tab-special tab-line-tab-inactive tab-line-tab-inactive-alternate tab-line-tab-modified
       tab-bar-tab tab-bar-tab-inactive tab-bar-tab-ungrouped)
      :box (:line-width (9 . 7) :color nil :style flat-button))
    '(tab-line-tab-group
      :box (:line-width (9 . 7) :color nil :style flat-button) :inherit tab-line)))

(use-package! tab-line
  :defer t
  :defer-incrementally t
  :ghook ('doom-first-buffer-hook #'global-tab-line-mode)
  :config
  ;; TODO: fix auto hscrolling
  ;; https://lists.gnu.org/r/bug-gnu-emacs/2020-02/msg00626.html

  (setq! tab-line-close-button (propertize "  ✕"
                                           'keymap tab-line-tab-close-map
                                           'mouse-face 'tab-line-close-highlight
                                           'help-echo "Click to close tab")
         tab-line-new-button   (propertize "+"
                                           'keymap tab-line-add-map
                                           'mouse-face 'tab-line-highlight
                                           'face '(:foreground "gray")
                                           'help-echo "Click to add tab")
         ;; https://7d.nz/Init-org.html#tabline
         tab-line-left-button  (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                           'keymap tab-line-left-map
                                           'mouse-face 'tab-line-highlight
                                           'face '(:foreground "gray")
                                           'help-echo "Click to scroll left")
         tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                           'keymap tab-line-right-map
                                           'mouse-face 'tab-line-highlight
                                           'face '(:foreground "gray")
                                           'help-echo "Click to scroll right"))

  (map! [tab-line mouse-4]     #'+tabs/hscroll-left
        [tab-line mouse-5]     #'+tabs/hscroll-right
        [tab-line wheel-up]    #'+tabs/hscroll-left
        [tab-line wheel-down]  #'+tabs/hscroll-right
        [tab-line wheel-left]  #'+tabs/hscroll-left
        [tab-line wheel-right] #'+tabs/hscroll-right)

  (setq! tab-line-tab-name-function #'+tabs-name-fn
         ;; TODO: try `tab-line-tabs-fixed-window-buffers' (the default in 30+)
         tab-line-tabs-function #'+tabs-fn)

  ;; (map! :when (boundp 'tab-line-mode-map)
  ;;       :map tab-line-mode-map
  ;;       "s-[" #'tab-line-switch-to-prev-tab
  ;;       "s-]" #'tab-line-switch-to-next-tab)

  (add-hook! '(+popup-buffer-mode-hook doom-switch-window-hook doom-switch-buffer-hook)
             #'+tabs-maybe-add-tab-line-for-popup-buffer)

  (pushnew! tab-line-exclude-modes '+doom-dashboard-mode)

  (defadvice! +tabs--undedicate-a (fn &optional event &rest args)
    :around #'tab-line-select-tab
    :around #'tab-line-new-tab
    :around #'tab-line-close-tab
    :around #'tab-line-switch-to-next-tab
    :around #'tab-line-switch-to-prev-tab
    (let ((w (and event
                  (eventp event)
                  (event-start event)
                  (posn-window (event-start event)))))
      (with-window-non-dedicated (and (windowp w) w)
        (apply fn event args)))))
