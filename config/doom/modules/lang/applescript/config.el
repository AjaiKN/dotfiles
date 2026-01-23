;;; lang/applescript/config.el -*- lexical-binding: t; -*-

(use-package! apples-mode
  :defer t
  :config
  (setq! apples-indent-offset (default-value 'tab-width)))

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
