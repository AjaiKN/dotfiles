;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("\\`%PDF" . pdf-view-mode)
  :init
  (after! pdf-annot
    (defun +pdf-cleanup-windows-h ()
      "Kill left-over annotation buffers when the document is killed."
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))
    (add-hook! 'pdf-view-mode-hook
      (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

  :config
  (defadvice! +pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (and (require 'pdf-info nil t)
             (or (pdf-info-running-p)
                 (ignore-errors (pdf-info-check-epdfinfo) t)))
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      ;;ADDED Fall back to doc-view-mode instead of fundamental-mode
      ;;(doc-view-fit-page-to-window seems to error if we call doc-view-mode immediately, so run-with-timer):
      (run-with-timer 0 nil #'doc-view-mode)
      (message "Viewing PDFs in Emacs with pdf-tools requires epdfinfo. Use `M-x pdf-tools-install' to build it.")))

  ;; Despite its namesake, this does not call `pdf-tools-install', it only sets
  ;; up hooks, auto-mode-alist/magic-mode-alist entries, global modes, and
  ;; refreshes pdf-view-mode buffers, if any.
  ;;
  ;; I avoid calling `pdf-tools-install' directly because `pdf-tools' is easy to
  ;; prematurely load in the background (e.g. when exporting an org file or by
  ;; packages like org-pdftools). And I don't want pdf-tools to suddenly block
  ;; Emacs and spew out compiler output for a few minutes in those cases. It's
  ;; abysmal UX. The `pdf-view-mode' advice above works around this with a less
  ;; cryptic failure message, at least.
  (pdf-tools-install-noverify)

  (map! :map pdf-view-mode-map
        :gn "q" #'kill-current-buffer ; For consistency with other special modes
        :mniev "<right>" #'pdf-view-next-page-command
        :mniev "<left>"  #'pdf-view-previous-page-command
        :mniev "_"       #'pdf-view-shrink)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Handle PDF-tools related popups better
  (set-popup-rules!
    '(("^\\*Outline*" :side right :size 40 :select nil)
      ("^\\*Edit Annotation " :quit nil)
      ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; HACK Refresh FG/BG for pdfs when `pdf-view-midnight-colors' is changed by a
  ;;      theme or with `setq!'.
  (defun +pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (put 'pdf-view-midnight-colors 'custom-set
       (lambda (sym value)
         (set-default sym value)
         (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
           (with-current-buffer buffer
             (if (get-buffer-window buffer)
                 (+pdf-reload-midnight-minor-mode-h)
               ;; Defer refresh for buffers that aren't visible, to avoid
               ;; blocking Emacs for too long while changing themes.
               (add-hook 'doom-switch-buffer-hook #'+pdf-reload-midnight-minor-mode-h
                         nil 'local))))))

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw))))


(use-package! saveplace-pdf-view
  :after pdf-view)


;; Often when I have a pdf open in emacs, it's changing regularly and
;; I want it to update immediately when it changes.
;; (e.g. compiling from latex)
(add-hook! '(pdf-view-mode-hook doc-view-mode-hook)
           #'auto-revert-mode)
(setq-hook! '(pdf-view-mode-hook doc-view-mode-hook)
  auto-revert-interval 0.4)


(use-package! doc-view
  :defer t
  :config
  (map! :map doc-view-mode-map
        :nm "-" #'doc-view-shrink
        :nm "_" #'doc-view-shrink
        :nm "k" #'doc-view-previous-page
        :nm "j" #'doc-view-next-page)

  (add-hook! 'doc-view-mode-hook
             #'doc-view-fit-page-to-window)
  (setq-hook! 'doc-view-mode-hook
    doc-view-continuous t))
