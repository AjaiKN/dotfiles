;;; app/omni/config.el -*- lexical-binding: t; -*-

(use-package! consult-omni
  :defer t
  ;; :defer-incrementally t
  :config
  (setq! consult-omni-alternate-browse-function #'xwidget-webkit-browse-url
         consult-omni-default-interactive-command #'consult-omni-multi
         ;; consult-omni-default-autosuggest-command #'consult-omni-dynamic-brave-autosuggest
         consult-omni-show-preview t
         consult-omni-default-preview-function #'xwidget-webkit-browse-url
         consult-omni-preview-key "C-SPC"))

(after! (:and embark consult-omni)
  (require 'consult-omni-embark))

(use-package! consult-omni-sources
  :defer t
  :init
  (add-transient-hook! #'consult-omni-multi (require 'consult-omni-sources))
  ;; See consult-omni-sources--all-modules-list
  (setq consult-omni-sources-modules-to-load
        '(;; Local
          consult-omni-apps
          consult-omni-grep
          consult-omni-line-multi
          ;; consult-omni-locate          ;TODO: re-enable after locate db generated
          ;; consult-omni-man             ;seems to be very slow
          consult-omni-mdfind
          ;; consult-omni-mu4e
          ;; consult-omni-notmuch
          consult-omni-browser-history ;not working right now
          consult-omni-buffer
          consult-omni-calc
          consult-omni-fd
          ;; consult-omni-find
          consult-omni-ripgrep
          ;; consult-omni-ripgrep-all ;https://github.com/phiresky/ripgrep-all
          ;; consult-omni-elfeed
          consult-omni-git-grep

          ;; Org
          ;; consult-omni-org-agenda
          ;; consult-omni-consult-notes
          ;; consult-omni-notes

          ;; Online
          ;; consult-omni-bing
          ;; consult-omni-brave-autosuggest ;may need API key
          ;; consult-omni-brave             ;may need API key
          consult-omni-doi
          consult-omni-duckduckgo
          consult-omni-gh
          ;; consult-omni-google ;doesn't seem to be working right now
          ;; consult-omni-dict   ;works, but I don't think the suggestions are working
          ;; consult-omni-google-autosuggest

          ;; AI
          ;; consult-omni-chatgpt           ;need API key
          ;; consult-omni-gptel             ;need API key

          ;; consult-omni-pubmed            ;may need API key
          ;; consult-omni-stackoverflow     ;may need API key

          ;; consult-omni-numi
          ;; consult-omni-scopus            ;may need API key
          ;; consult-omni-youtube
          ;; consult-omni-invidious

          consult-omni-wikipedia))

  (setq consult-omni-multi-sources
        '(;; "buffers text search"
                                        ;"Browser History"
                                        ;"StackOverflow"
                                        ;"Scopus"
                                        ;"ripgrep"
                                        ;"PubMed"
                                        ;"Org Agenda"
                                        ;"Numi"
                                        ;"Notes Search"
                                        ;"ripgrep-all"
                                        ;"man"
          ;; "mdfind"
          "locate"
                                        ;"gptel"
          ;; "git-grep"
                                        ;"grep"
                                        ;"GitHub"
                                        ;"find"
          "fd"
                                        ;"elfeed"
          "DuckDuckGo API"
          "doiorg"
                                        ;"Dictionary"
                                        ;"chatGPT"
          "calc"
          "Bookmark"
          ;; "Project File"
          ;; "File"
          ;; "Project Buffer"
          ;; "Hidden Buffer"
          ;; "Modified Buffer"
          "Buffer"
                                        ;"Brave"
                                        ;"Brave AutoSuggest"
          "Apps"
          "Wikipedia"
          "Google"))

  ;; (doom-load-packages-incrementally consult-omni-sources-modules-to-load)
  :config
  (consult-omni-sources-load-modules))

;; https://github.com/armindarvish/consult-omni/wiki/YouTube-Tutorial
(defun consult-omni-demo-omni ()
  (interactive)
  (let* ((width (floor (* 0.8 (display-pixel-width))))
         (height (floor (* 0.8 (display-pixel-height))))
         (left  (floor (* 0.1 (display-pixel-width))))
         (top (floor (* 0.1 (display-pixel-height))))
         (params `((name . "demo-omni")
                   (width . ,(cons 'text-pixels width))
                   (height . ,(cons 'text-pixels height))
                   (left . ,left)
                   (top . ,top)
                   (alpha . (0.98 0.85))
                   (minibuffer . t)))
         (frame (make-frame params)))
    (with-selected-frame frame
      (select-frame-set-input-focus frame)
      (let* ((buffer (get-buffer-create "consult-omni-dashbaord")))
        (switch-to-buffer buffer)
        (setq-local fill-column 80)
        (display-line-numbers-mode -1)
        (erase-buffer)
        ;; (insert "\n\n\n\n\n\n\n\n\n\n")
        ;; (insert "\n\n\n")
        ;; (insert (concat (propertize "What you " 'face 'font-lock-function-call-face)
        ;;                 (propertize "seek" 'face 'font-lock-variable-name-face))
        ;;         (propertize " is seeking you!" 'face 'font-lock-function-call-face))
        ;; (insert (concat "\n\n"
        ;;                 (propertize "preview:" 'face 'font-lock-doc-face)
        ;;                 "\s\s"
        ;;                 (propertize "C-o" 'face 'font-lock-type-face)
        ;;                 "\s\s\s\s\s\s\s\s"
        ;;                 (propertize "narrow:" 'face 'font-lock-doc-face)
        ;;                 "\s\s"
        ;;                 (propertize "M-o" 'face 'font-lock-type-face)
        ;;                 "\s\s\s\s\s\s\s\s"
        ;;                 (propertize "move:" 'face 'font-lock-doc-face)
        ;;                 "\s\s"
        ;;                 (propertize "C- j k h l" 'face 'font-lock-type-face)))
        (save-excursion
          (center-line -3))
        (recenter nil t)
        (text-scale-set 1.4)
        (setq-local cursor-type '(nil))
        (setq-local evil-normal-state-cursor '(nil))
        (setq-local evil-insert-state-cursor '(nil))
        (delete-other-windows)
        (redisplay)
        (consult-omni-multi)))))

(defvar vertico-count) ;silence warning
(defun consult-omni-demo-launcher ()
  (interactive)
  ;; (require 'yequake)
  (let* (;(resize-mini-frames #'yequake-fit-frame-vertically)
         (vertico-count 30)
         (width-portion 0.5)
         (height-portion 0.5)
         (width (floor (* width-portion (display-pixel-width))))
         (height (floor (* height-portion (display-pixel-height))))
         (left  (floor (* (/ (- 1 width-portion) 2) (display-pixel-width))))
         (top (floor (* (/ (- 1 height-portion) 2) (display-pixel-height))))
         (params `((name . "demo-omni")
                   (width . ,(cons 'text-pixels width))
                   (height . ,(cons 'text-pixels height))
                   (left . ,left)
                   (top . ,top)
                   (minibuffer . only)
                   (undecorated . t)))
         (frame (make-frame params)))
    (with-selected-frame frame
      (akn/focus-this-frame)  ; TODO: this is defined in my private config
      (unwind-protect
          (progn (require 'consult-omni-apps)
                 (consult-omni-apps-static ".*" (propertize "  " 'face 'consult-omni-path-face))
                 nil)
        (progn
          (when (frame-live-p frame) (delete-frame frame))
          nil)))))

(use-package! browser-hist
  :defer t
  :commands (browser-hist-search)
  :config
  (require 'embark)
  (setq browser-hist-default-browser 'firefox)
  (setf (alist-get 'firefox browser-hist-db-paths)
        "/Users/ajainelson/Library/Application Support/Firefox/Profiles/g85mnrh5.default/places.sqlite"))
