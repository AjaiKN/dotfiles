;;; editor/mindstream/config.el -*- lexical-binding: t; -*-

(defvar-keymap akn/mindstream-map)
(map! "M-s-n" #'mindstream-new
      :desc "mindstream" "C-c ," akn/mindstream-map
      (:leader :desc "mindstream" "y" akn/mindstream-map)

      :map akn/mindstream-map
      "n"   #'mindstream-new
      "b"   #'mindstream-enter-anonymous-session
      "t"   #'mindstream-enter-session-for-template
      "m"   #'mindstream-begin-session
      "q"   #'mindstream-end-session
      "s"   #'mindstream-save-session
      "C-s" #'mindstream-save-session
      "r"   #'mindstream-load-session
      "a"   #'mindstream-archive
      "C-l" #'mindstream-go-live
      "C-o" #'mindstream-go-offline)

(use-package! mindstream
  :commands (mindstream-new mindstream-enter-anonymous-session mindstream-enter-session-for-template mindstream-begin-session mindstream-end-session mindstream-save-session mindstream-save-session mindstream-load-session mindstream-archive mindstream-go-live mindstream-go-offline)
  :config
  (setq mindstream-template-path (expand-file-name "mindstream-templates/" doom-user-dir)
        mindstream-path (akn/expand-file "~/mindstream/anon")
        mindstream-archive-path (akn/expand-file "~/mindstream/archive")
        mindstream-save-session-path (akn/expand-file "~/mindstream/saved"))
  (dolist (dir (list mindstream-template-path mindstream-path mindstream-archive-path mindstream-save-session-path))
    (mkdir dir t))
  (mindstream-mode))
