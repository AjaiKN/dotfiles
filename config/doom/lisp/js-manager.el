;;; js-manager.el --- WIP: Run commands for JavaScript package managers (npm, yarn, pnpm, bun) -*- lexical-binding: t; -*-

;;; Commentary:
;; WIP

;;; Code:

;; IDEA: could use `tabulated-list-mode' or `tablist-mode' for dependencies

(require 'transient)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'json)

(defun js-manager/package-json-deps ()
  (when-let* ((json (js-manager/package-json))
              ((listp json))
              (deps (let-alist json
                      (append .dependencies
                              .devDependencies
                              .peerDependencies
                              .bundledDependencies
                              .optionalDependencies))))
    (seq-filter (lambda (p)
                  (and (consp p)
                       (or (symbolp (car p)) (stringp (car p)))
                       (stringp (cdr p))))
                deps)))
(defun js-manager/package-json-depnames ()
  (mapcar (lambda (x)
            (format "%s" (car x)))
          (js-manager/package-json-deps)))
(defun js-manager/package-json-scripts ()
  (alist-get 'scripts (js-manager/package-json)))
(defun js-manager/package-json-script-names ()
  (mapcar (lambda (x)
            (format "%s" (car x)))
          (js-manager/package-json-scripts)))
(defun js-manager/complete-scripts (prompt)
  (let* ((scripts (js-manager/package-json-scripts))
         ;; https://www.npmjs.com/package/npm-scripts-info
         (script-descriptions (append (alist-get 'scripts-info (js-manager/package-json))
                                      scripts))
         (longest-length (and scripts
                              (apply #'max (mapcar (lambda (x) (length (symbol-name (car x)))) scripts))))
         (completion-extra-properties `(:annotation-function
                                        ,(lambda (str)
                                           (concat (make-string (+ (- longest-length (length str)) 3) ?\s)
                                                   (or (alist-get str script-descriptions nil nil #'equal)
                                                       (alist-get (intern str) script-descriptions)
                                                       ""))))))
    (completing-read prompt scripts)))

(defun js-manager/complete-installed-packages (prompt)
  (let* ((deps (js-manager/package-json-deps))
         (completion-extra-properties `(:annotation-function
                                        ,(lambda (str)
                                           (concat " "
                                                   (or (alist-get str deps nil nil #'equal)
                                                       (alist-get (intern str) deps)
                                                       ""))))))
    (completing-read-multiple prompt deps)))
(defun js-manager/remove (packages)
  (interactive (list (js-manager/complete-installed-packages "Packages to remove: ")))
  (message "nun %s" (string-join packages " ")))

(defun js-manager/all-known-packages-pnpm-cache ()
  (let ((pnpm-pkgs (and (file-exists-p "~/.pnpm-store/v3/metadata/registry.npmjs.org")
                        (file-directory-p "~/.pnpm-store/v3/metadata/registry.npmjs.org")
                        (directory-files-recursively "~/.pnpm-store/v3/metadata/registry.npmjs.org" (rx)))))
    (mapcar (lambda (file)
              (and (string-match (rx bos (literal "~/.pnpm-store/v3/metadata/registry.npmjs.org") "/" (group (one-or-more not-newline)) ".json" eos)
                                 file)
                   (match-string-no-properties 1 file)))
            pnpm-pkgs)))
(defun js-manager/all-known-packages ()
  (append (js-manager/all-known-packages-pnpm-cache)
          (js-manager/package-json-depnames)))
          ;; kind of slow
          ;;(js-manager/local-node-modules)))
          ;;(js-manager/global-npm-node-modules)))
(defun js-manager/complete-any-packages (prompt)
  (let* ((all-packages (js-manager/all-known-packages)))
    (completing-read-multiple prompt all-packages)))

;;(directory-files-recursively (shell-command-to-string "npm prefix -g"))

(defun js-manager/node-modules-pkgs (dir)
  (directory-files-recursively
   dir
   (rx)
   t
   (lambda (dir)
     ;; https://emacs.stackexchange.com/a/10295
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Waiting.html
     ;; let emacs check for timeout
     (sit-for 0)
     (string-match-p (rx (* not-newline) "node_modules/"
                         (group (or (seq "@" (+ (any alphanumeric "-")))
                                    (seq "@" (+ (any alphanumeric "-")) "/" (+ (any alphanumeric "-")))
                                    (+ (any alphanumeric "-"))))
                         (? "/node_modules")
                         eos)
                     dir))))



(defun js-manager/node-modules-pkgs2 (dir)
  (seq-filter #'identity (mapcar (lambda (f)
                                   (and (string-match
                                         (rx (* not-newline) "node_modules/"
                                             (group (or (seq "@" (+ (any alphanumeric "-")) "/" (+ (any alphanumeric "-")))
                                                        (+ (any alphanumeric "-"))))
                                             eol)
                                         f)
                                        (match-string-no-properties 1 f)))
                                 (js-manager/node-modules-pkgs dir))))

(defun js-manager/node-modules-pkgs3 (dir)
  (condition-case nil
      (with-timeout (10 nil)
        (with-local-quit
          (js-manager/node-modules-pkgs2 dir)))
    ;; the user quit
    ((quit) nil)
    ;; some other error
    (error nil)))

(defun js-manager/existing-file (f)
  (and f
       (file-exists-p f)
       f))

(defun js-manager/global-npm-node-modules ()
  (when-let* ((prefix (string-chop-newline (shell-command-to-string "npm prefix -g"))))
    (if-let* ((dir (js-manager/existing-file (concat prefix "/lib/node_modules"))))
        (js-manager/node-modules-pkgs3 dir)
      (when-let* ((dir (js-manager/existing-file (concat prefix "/node_modules"))))
        (js-manager/node-modules-pkgs3 dir)))))

(defun js-manager/local-node-modules ()
  (when-let* ((prefix (locate-dominating-file default-directory "node_modules"))
              (dir (concat prefix "/node_modules")))
    (js-manager/node-modules-pkgs3 dir)))

;; https://github.com/antfu/ni/blob/main/src/agents.ts
(defvar js-manager/locks
  '(("bun.lockb" . bun)
    ("pnpm-lock.yaml" . pnpm)
    ("yarn.lock" . yarn)
    ("package-lock.json" . npm)
    ("npm-shrinkwrap.json" . npm)))

(defun js-manager/lock ()
  (let (the-dir the-lock)
    (setq the-dir
          (locate-dominating-file
            default-directory
            (lambda (dir)
              (seq-some (lambda (lock)
                          (and (file-exists-p (expand-file-name (car lock) dir))
                               (progn (setq the-lock lock)
                                      t)))
                        js-manager/locks))))
    (and the-dir (cons the-dir the-lock))))


;; https://github.com/antfu/ni/blob/main/src/detect.ts
(defun js-manager/package-json-path (&optional lock-file)
  (when (not lock-file)
    (setq lock-file (car (js-manager/lock))))
  (or (when-let* ((dir (and lock-file (file-name-directory lock-file))))
        (js-manager/existing-file (expand-file-name "package.json" dir)))
      (when-let* ((dir (locate-dominating-file default-directory "package.json")))
        (js-manager/existing-file (expand-file-name "package.json" dir)))))
(defun js-manager/package-json (&optional _lock-file)
  (when-let* ((path (js-manager/package-json-path)))
    (ignore-errors
      (json-read-file path))))

(defun js-manager/npm-run (agent)
  (lambda (args)
    (if (> (length args) 1)
        (format "%s run %s -- %s" agent (car args) (string-join (cdr args) " "))
      (format "%s run %s" agent (car args)))))

(defvar js-manager/agents
  (let ((yarn '((agent "yarn %s")
                (run "yarn run %s")
                (install "yarn install %s")
                (frozen "yarn install --frozen-lockfile")
                (global "yarn global add %s")
                (add "yarn add %s")
                (upgrade "yarn upgrade %s")
                (upgrade-interactive "yarn upgrade-interactive %s")
                (execute "npx %s")
                (uninstall "yarn remove %s")
                (global-uninstall "yarn global remove %s")))
        (pnpm '((agent "pnpm %s")
                (run "pnpm run %s")
                (install "pnpm i %s")
                (frozen "pnpm i --frozen-lockfile")
                (global "pnpm add -g %s")
                (add "pnpm add %s")
                (upgrade "pnpm update %s")
                (upgrade-interactive "pnpm upgrade -i %s")
                (execute "pnpm dlx %s")
                (uninstall "pnpm remove %s")
                (global-uninstall "pnpm remove --global %s"))))
    `((npm
       (agent "npm %s")
       (run ,(js-manager/npm-run "npm"))
       (install "npm i %s")
       (frozen "npm ci")
       (global "npm i -g %s")
       (add "npm i %s")
       (upgrade "npm update %s")
       (execute "npx %s")
       (uninstall "npm uninstall %s")
       (global-uninstall "npm uninstall -g %s"))
      (yarn
       ,@yarn)
      (yarn@berry
       (frozen "yarn install --immutable")
       (upgrade "yarn up %s")
       (upgrade-interactive "yarn up -i %s")
       (execute "yarn dlx %s")
       (global "npm i -g %s")
       (global-uninstall "npm uninstall -g %s")
       ,@yarn)
      (pnpm
       ,@pnpm)
      (pnpm@6
       (run ,(js-manager/npm-run "pnpm")))
      (bun
       (agent "bun %s")
       (run "bun run %s")
       (install "bun install %s")
       (frozen "bun install --no-save")
       (global "bun add -g %s")
       (add "bun add %s")
       (upgrade "bun update %s")
       (upgrade-interactive "bun update %s")
       (execute "bunx %s")
       (uninstall "bun remove %s")
       (global-uninstall "bun remove -g %s")))))

(defun js-manager/install-agent-command (agent &optional version)
  (format "npm i -g %s%s"
          agent
          (if version (concat "@" version) "")))

;; https://github.com/antfu/ni/blob/main/src/detect.ts
(defun js-manager/detect-agent-version ()
  (let* ((lock (js-manager/lock))
         (lock-dir-path (car lock))
         (lock-agent (and (cdr lock) (cddr lock))))
    (or (ignore-errors
          (when-let* ((package-json-path (js-manager/package-json-path lock-dir-path))
                      (pkg (json-read-file package-json-path))
                      (package-manager (alist-get 'packageManager pkg nil nil #'eq)))
            (string-match (rx bos (? "^")
                              (group (minimal-match (* (not "@"))))
                              "@"
                              (group (minimal-match (* (not "@")))))
                          package-manager)
            (let ((name (match-string-no-properties 1 package-manager))
                  (ver  (match-string-no-properties 2 package-manager)))
              (cond
               ((and (equal name "yarn") (> (string-to-number ver) 1))
                (cons 'yarn@berry "berry"))
               ((and (equal name "pnpm") (< (string-to-number ver) 7))
                (cons 'pnpm@6 ver))
               ((member (intern name) (mapcar #'car js-manager/agents))
                (cons (intern name) ver))
               (t
                (warn "Unknown packageManager: %s" package-manager)
                nil)))))
        lock-agent)))

(defvar js-manager/autoinstall-agent 'prompt)
(defvar js-manager/install-pages
  '((bun . "https://bun.sh")
    (pnpm . "https://pnpm.io/installation")
    (pnpm@6 . "https://pnpm.io/6.x/installation")
    (yarn . "https://classic.yarnpkg.com/en/docs/install")
    (yarn@berry . "https://yarnpkg.com/getting-started/install")
    (npm . "https://docs.npmjs.com/cli/v8/configuring-npm/install")))
(defun js-manager/ensure-agent-installed (agent &optional version)
  (let ((agent-basic-name (car (string-split (symbol-name agent) "@"))))
    (when (and js-manager/autoinstall-agent
               agent
               (not (executable-find agent-basic-name)))
      (if (and (not (equal agent-basic-name "npm"))
               (yes-or-no-p (format "Package manager %s needs to be installed. Install it using `%s`?"
                                    agent-basic-name
                                    (js-manager/install-agent-command agent-basic-name version))))
          (message "%s" (js-manager/install-agent-command agent-basic-name version))
        (user-error "Executable %s not found. Please install it: %s" agent-basic-name (alist-get agent js-manager/install-pages))))))

(defun js-manager/detect ()
  (let* ((agent-version (js-manager/detect-agent-version))
         (agent (car agent-version))
         (version (cdr agent-version)))
    (js-manager/ensure-agent-installed agent version)
    agent))

(defun js-manager/quote (arg)
  (if (and (not (string-prefix-p "--" arg))
           (string-match-p " " arg))
      (json-serialize arg)
    arg))

(defun js-manager/get-command (agent command args)
  (let ((c (alist-get command (alist-get agent js-manager/agents))))
    (cond
     ((functionp c) (funcall c args))
     ((stringp c) (string-trim
                   (if (string-match-p (rx "%s") c)
                       (format c (string-join (mapcar #'js-manager/quote args) " "))
                     c)))
     (t (error "command %s unsupported for agent %s" command agent)))))

(defun js-manager/parse-ni (agent args has-lock)
  (when (eq agent 'bun)
    (setq args
          (mapcar (lambda (i) (if (equal i "-D") "-d" i))
                  args)))
  (cond
   ((member "-g" args)
    (js-manager/get-command agent 'global (remove "-g" args)))
   ((member "--frozen-if-present" args)
    (js-manager/get-command agent
                    (if has-lock 'frozen 'install)
                    (remove "--frozen-if-present" args)))
   ((member "--frozen" args)
    (js-manager/get-command agent 'frozen (remove "--frozen" args)))
   ((or (null args)
        (seq-every-p (apply-partially #'string-prefix-p "-") args))
    (js-manager/get-command agent 'install args))))

(defun js-manager/parse-nr (agent args _)
  (when (null args)
    (push "start" args))
  (when (member "--if-present" args)
    (setq args (delq "--if-present" args))
    (setq args (cons (format "--if-present %s" (car args))
                     (cdr args))))
  (js-manager/get-command agent 'run args))

(defun js-manager/parse-nun (agent args _)
  (if (member "-g" args)
      (js-manager/get-command agent 'global-uninstall (remove "-g" args))
    (js-manager/get-command agent 'uninstall args)))

(defun js-manager/parse-nlx (agent args _)
  (js-manager/get-command agent 'execute args))

(defun js-manager/parse-na (agent args _)
  (js-manager/get-command agent 'agent args))

(defvar js-manager/default-agent 'prompt)
(defvar js-manager/global-agent 'prompt)

(defun js-manager/get-cli-command (fn args)
  (let ((agent (if (member "-g" args)
                   js-manager/global-agent
                 (or (js-manager/detect) js-manager/default-agent))))
    (when (eq agent 'prompt)
      (setq agent
            (completing-read "Choose the agent: "
                             (seq-filter (lambda (i) (not (string-match-p (rx "@") i)))
                                         (mapcar #'car js-manager/agents))
                             nil
                             t)))
    (funcall fn args agent)))

(defun js-manager/run-cli (fn &optional args)
  (when (stringp args)
    (setq args (split-string-shell-command args)))
  (let ((default-directory (if (equal (car args) "-C")
                               (progn
                                 (file-truename (cadr args))
                                 (setq args (cddr args)))
                             default-directory)))
    (with-existing-directory
      (let ((command (js-manager/get-cli-command fn args)))
        (format "Run command: %s" command)))))

(defun js-manager/command ()
  (interactive)
  (message "command"))
(defun js-manager/install ()
  (interactive)
  (message "install"))
;; (defun js-manager/ci ()
;;   (interactive)
;;   (message "ci"))
(defun js-manager/add ()
  (interactive)
  (message "add"))
(defun js-manager/upgrade ()
  (interactive)
  (message "upgrade"))
(defun js-manager/run ()
  (interactive)
  (message "run"))
(defun js-manager/x ()
  (interactive)
  (message "x"))
(transient-define-prefix ni ()
  ["General"
   ("n" "Type a command manually" js-manager/command)]
  ["Dependencies"
   ("i" "install declared deps" js-manager/install)
   ;; Use --frozen instead like ni
   ;; ("c" "ci (clean install) declared deps" js-manager/ci)
   ("a" "add/remove dep" js-manager/add)
   ("u" "update/upgrade dep" js-manager/upgrade)]
  ["Running commands"
   ("r" "run" js-manager/run)
   ("x" "download & execute" js-manager/x)])
  ;; ["Less common commands"
  ;;  ("a" "audit" js-manager/audit)
  ;;  ("I" "init" js-manager/init)
  ;;  ("l" "ls" js-manager/ls)
  ;;  ("L" "link" js-manager/link)
  ;;  ("o" "outdated" js-manager/outdated)
  ;;  ("p" "prune" js-manager/prune)
  ;;  ("P" "publish" js-manager/publish)])


(provide 'js-manager)
;;; js-manager.el ends here
