;;; tools/mise/init.el -*- lexical-binding: t; -*-

;; https://mise.jdx.dev/ide-integration.html#traditional-shims-way

;; CLI tools installed by Mise
;; See: https://www.emacswiki.org/emacs/ExecPath
(let* ((shims-path (concat (getenv "HOME") "/.local/share/mise/shims"))
       (with-colon (concat shims-path ":")))
  ;; add shims to front of PATH (if it's not already there at the front)
  (when (not (string-prefix-p with-colon (getenv "PATH")))
    (setenv "PATH" (concat with-colon (getenv "PATH"))))
  ;; add shims to exec-path
  (when (not (equal (car exec-path) shims-path))
    (setq! exec-path (cons shims-path exec-path)))
  ;; remove any other Mise paths from exec-path
  (setq! exec-path
         (seq-filter (lambda (p) (not (string-match-p "local/share/mise/installs" p)))
                     exec-path)))
