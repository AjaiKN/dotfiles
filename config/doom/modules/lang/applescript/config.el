;;; lang/applescript/config.el -*- lexical-binding: t; -*-

(use-package! applescript-mode
  :commands (+applescript-mode)
  :config
  (define-derived-mode +applescript-mode prog-mode "AppleScript"
    "A replacement wrapper for `applescript-mode'.

This uses `define-derived-mode', as opposed to the old style of defining
major modes, where regular functions were used."
    :group 'applescript
    (akn/letf! ((major-mode major-mode) ; don't change `major-mode' from `+applescript-mode' to `applescript-mode'
                (mode-name mode-name)
                (#'kill-all-local-variables #'ignore)) ; with define-derived-mode, there's no need to kill-all-local-variables
      (applescript-mode)))
  (derived-mode-add-parents '+applescript-mode '(applescript-mode))
  :init
  (when (boundp 'major-mode-remap-defaults)
    (setf (alist-get 'applescript-mode major-mode-remap-defaults) #'+applescript-mode)))

;; By default, Script Editor.app saves AppleScript in the compiled ".scpt"
;; format (https://apple.stackexchange.com/q/163134) rather than as plain text
;; ".applescript" files. So I want to automatically compile/decompile the
;; ".scpt" files like Script Editor does.
(when (executable-find "osadecompile")
  (add-to-list 'jka-compr-compression-info-list
               ;;[regexp
               ;; compr-message  compr-prog  compr-args
               ;; uncomp-message uncomp-prog uncomp-args
               ;; can-append strip-extension-flag file-magic-bytes
               ;; uncompress-function]
               `[,(rx ".scpt" eos)
                 "compiling AppleScript to binary .scpt file" "osacompile" ("-o" "/dev/stdout")
                 "decompiling binary .scpt file to AppleScript text" ,(expand-file-name "osadecompile-stdin" (dir!)) ()
                 nil nil "FasdUAS"
                 nil])
  (jka-compr-update))
