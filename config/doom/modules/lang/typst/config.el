;;; lang/typst/config.el -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'akn-doom-use-package)
  (require 'doom-keybinds)
  (require 'akn))

(use-package! typst-ts-mode
  :defer t
  :defer-incrementally (treesit
                        typst-ts-core typst-ts-embedding-lang-settings
                        typst-ts-faces
                        compile typst-ts-compile typst-ts-watch-mode
                        typst-ts-embedding-lang-settings edit-indirect typst-ts-edit-indirect
                        outline typst-ts-editing
                        eglot typst-ts-lsp
                        typst-ts-misc-commands
                        ;; transient typst-ts-transient ;was causing an error
                        typst-ts-compile typst-ts-misc-commands)

  :init
  (set-tree-sitter! 'typst-mode 'typst-ts-mode
    '((typst :url "https://github.com/uben0/tree-sitter-typst")))
  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
  :config
  (map! :map typst-ts-mode-map
       "C-c C-w" #'typst-ts-watch-mode
       ;; "C-c C-o" #'typst-ts-compile
       (:localleader "w" #'typst-ts-watch-mode))
                     ;; "o" #'typst-ts-compile))

  (setq! typst-ts-grammar-location (car (doom-glob doom-cache-dir "tree-sitter/libtree-sitter-typst.*")))

  (setq! typst-ts-watch-options '("--open")
         typst-ts-fontification-precision-level 'max)
  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(typst-ts-mode default typst-ts-indent-offset)))
  (after! editorconfig
    (add-to-list 'editorconfig-indentation-alist '(typst-ts-mode typst-ts-indent-offset)))
  (after! lsp-mode
    (add-to-list 'lsp--formatting-indent-alist '(typst-ts-mode . typst-ts-indent-offset)))

  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
    (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                                          :activation-fn (lsp-activate-on "typst")
                                          :major-modes '(typst-ts-mode)
                                          :server-id 'typst_tinymist)))

  (when (executable-find "tinymist")
    (set-eglot-client! 'typst-ts-mode "tinymist"))

  ;; (add-hook! 'typst-ts-mode-hook
  ;;   (defun +typst--turn-off-electric-indent ()
  ;;     (electric-indent-local-mode -1)))

  (add-hook 'typst-ts-mode-local-vars-hook #'lsp!)

  (defadvice! +typst-ts--watch-process-filter-revert-a (_proc output)
    :after #'typst-ts--watch-process-filter
    (when (string-match-p (rx "compiled") output)
      (run-hooks '+typst-watch-compiled-hook)))

  (add-hook! '(typst-ts-compile-after-compilation-hook +typst-watch-compiled-hook)
    (defun +typst--revert-pdfs-h (&rest _)
      (walk-windows
       (lambda (window)
         (with-selected-window window
           (when (and buffer-file-name
                      (string-match-p (rx ".pdf" eos) buffer-file-name)
                      (file-exists-p (replace-regexp-in-string (rx ".pdf" eos) ".typ" buffer-file-name))
                      (not (buffer-modified-p))
                      (not (file-remote-p buffer-file-name)))
             (revert-buffer))))
             ;;(message "Reverted %s!!" buffer-file-name))))
       nil
       'visible)))

  (after! smartparens
    ;; https://typst.app/docs/reference/syntax/
    (sp-with-modes 'typst-ts-mode
      (sp-local-pair "+" "+" :actions '() :when nil)
      (sp-local-pair "|" "|" :actions '() :when nil)
      (sp-local-pair "<" ">" :actions '(:add wrap autoskip) :when nil)
      (sp-local-pair "=" "=" :actions '() :when nil)
      (sp-local-pair "/" "/" :actions '() :when nil)
      (sp-local-pair "~" "~" :actions '() :when nil)
      (sp-local-pair "*" "*" :actions '(:add insert wrap autoskip navigate) :when nil
                     :unless (list #'+typst-sp--in-math-p
                                   #'sp-point-after-word-p
                                   #'sp-point-before-word-p))
      (sp-local-pair "_" "_" :actions '(:add insert wrap autoskip navigate) :when nil
                     :unless (list #'+typst-sp--in-math-p
                                   #'sp-point-after-word-p
                                   #'sp-point-before-word-p))
      (sp-local-pair "`" "`" :actions '(:add insert wrap autoskip navigate) :when nil
                     :unless (list #'+typst-sp--in-math-p))
      (sp-local-pair "$" "$" :actions '(:add insert wrap autoskip navigate) :when nil))

    (defun +typst-sp--in-math-p (_id _action _context)
      (cl-block nil
        (when (derived-mode-p 'typst-ts-mode)
          (let ((node (treesit-node-at (point))))
            (while node
              (when (member (treesit-node-type node) '("math" "formula"))
                (cl-return t))
              (setq node (treesit-node-parent node)))
            nil))))))

  ;; (remove-hook! 'typst-ts-mode-hook
  ;;   (defun +typst--hack-thing-h ()
  ;;     (interactive)
  ;;     (save-restriction
  ;;       (widen)
  ;;       (treesit-font-lock-fontify-region (point-min) (point-max) t))
  ;;     (akn/after-idle! (0)
  ;;       (treesit-font-lock-fontify-region (point-min) (point-max) t)))))

(use-package! typst-preview
  :defer t
  :defer-incrementally (websocket json)
  :init
  (defun +typst/preview-this-file ()
    (interactive nil typst-ts-mode)
    (akn/letf! ((typst-preview-autostart t)
                (typst-preview-ask-if-pin-main nil)
                (define-advice read-file-name (:around (read-file-name prompt &optional dir default-filename &rest args) akn/typst-a)
                  (if (and default-filename (not args))
                      (expand-file-name default-filename dir)
                    (apply read-file-name prompt dir default-filename args))))
      (typst-preview-mode)))
  (defun +typst/preview-browser ()
    (interactive nil typst-ts-mode)
    (when (not (bound-and-true-p typst-preview-mode))
      (+typst/preview-this-file))
    (save-selected-window
      (let ((display-buffer-overriding-action (cons nil '((inhibit-same-window . t) (window-width . 0.5))))
            (switch-to-buffer-obey-display-actions t))
        (akn/letf! ((define-advice xwidget-webkit-browse-url (:filter-args (args))
                      ;; always new-session
                      (cons (car args)
                            (cons t
                                  (cddr args)))))
          (tp--connect-browser typst-preview-browser (tp--master-static-host tp--local-master))))))
  (map! :map typst-ts-mode-map
        "C-c C-j" (akn/cmds! (bound-and-true-p typst-preview-mode)
                         #'typst-preview-send-position
                         #'+typst/preview-this-file)
        "C-c C-o" #'+typst/preview-browser)
  :config
  (setq! typst-preview-browser "default")
  ;; (when (fboundp 'xwidget-webkit-browse-url)
  ;;   (setq! typst-preview-browser "xwidget"))
  (setq! typst-preview-invert-colors "never"))
