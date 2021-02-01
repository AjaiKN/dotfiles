;;; tools/llm/config.el -*- lexical-binding: t; -*-

(require 'akn-doom-use-package)

(defun akn/chatgpt-api-key ()
  (auth-source-pick-first-password :host "api.openai.com" :user "apikey" :require '(:secret)))
(defun akn/gemini-api-key ()
  (auth-source-pick-first-password :host "generativelanguage.googleapis.com" :user "apikey" :require '(:secret)))

(add-hook! 'doom-after-modules-config-hook
  (if (boundp 'doom-leader-open-map)
      (defvar akn/doom-llm-gptel-map (keymap-lookup doom-leader-open-map "l"))
    (warn "llm-extra: doom-leader-open-map doesn't exist"))
  (map!
   :leader
   (:prefix ("l". "llm")
    :desc "gptel" "g" akn/doom-llm-gptel-map
    (:prefix ("e" . "evedel")
     ;; TODO: modification
     :desc "Create reference"                      "r" #'evedel-create-reference
     :desc "Create directive"                      "d" #'evedel-create-directive
     :desc "Process directives"                    "e" #'evedel-process-directives
     :desc "Preview prompt for directive at point" "p" #'evedel-preview-directive-prompt
     :desc "Delete instructions at point/region"   "k" #'evedel-delete-instructions
     :desc "Delete all instructions"               "K" #'evedel-delete-all
     :desc "Save instructions"                     "s" #'evedel-save-instructions
     :desc "Load instructions"                     "L" #'evedel-load-instructions)
    :desc "aidermacs" "a" #'aidermacs-transient-menu
    :desc "ellama" "l" (cmd!
                        (require 'ellama)
                        ;; copied from `+default/lsp-command-map'
                        (setq prefix-arg current-prefix-arg
                              unread-command-events
                              (mapcar (lambda (e) (cons t e))
                                      (vconcat (when (bound-and-true-p evil-this-operator)
                                                 (where-is-internal evil-this-operator
                                                                    evil-normal-state-map
                                                                    t))
                                               (this-command-keys)))))
    (:prefix ("s" . "chatgpt-shell")
     "s" #'chatgpt-shell
     "m" #'chatgpt-shell-swap-model
     "M" #'chatgpt-shell-model-version
     "c" #'chatgpt-shell-prompt-compose
     "i" #'chatgpt-shell-describe-image)
    (:prefix ("c" . "copilot")
     "d" #'copilot-diagnose
     "l" #'copilot-login
     "c" #'copilot-mode
     "TAB" #'copilot-complete
     "a" #'copilot-accept-completion
     "x" #'copilot-clear-overlay
     "W" #'copilot-accept-completion-by-line
     "w" #'copilot-accept-completion-by-word
     "n" #'copilot-next-completion
     "p" #'copilot-previous-completion
     "L" #'copilot-logout)
    (:prefix ("x" . "semext")
     "f" #'semext-forward-part
     "b" #'semext-backward-part
     "%" #'semext-query-replace
     "s" #'semext-search-forward
     "r" #'semext-search-backward))))

;;; gptel (https://github.com/karthink/gptel) - supports many
(use-package! gptel
  :defer t
  :config
  (setq! gptel-api-key #'akn/chatgpt-api-key)

  (gptel-make-gemini "Gemini"
    :key #'akn/gemini-api-key
    :stream t))

;;;; evedel

;;; ellama (https://github.com/s-kostyaev/ellama) - supports many
(use-package! ellama
  :defer t
  :config
  (require 'llm-openai)
  (require 'llm-gemini)
  ;; :default-chat-temperature :default-chat-max-tokens
  (setq! ellama-provider
         (cdar akn/llms-alist)
         ellama-providers
         (cdr akn/llms-alist)))
(after! ellama
  (map! :leader "l l" nil)
  (map! :leader "l l" ellama-command-map)
  (map! :leader
        :prefix "l l"
        (:prefix ("c" . "code/commit"))
        (:prefix ("s" . "summarize/session"))
        (:prefix ("i" . "improve"))
        (:prefix ("m" . "make"))
        (:prefix ("a" . "ask"))
        (:prefix ("t" . "translate"))
        (:prefix ("d" . "summarize/session"))
        (:prefix ("x" . "context-add"))
        (:prefix ("p" . "provider"))))

;;; chatgpt-shell (https://github.com/xenodium/chatgpt-shell) - supports multiple
(use-package! chatgpt-shell
  :defer t
  :config
  (setq! chatgpt-shell-openai-key #'akn/chatgpt-api-key)
  (add-hook 'chatgpt-shell-mode-hook #'doom-mark-buffer-as-real-h))

;;; llm (https://github.com/ahyatt/llm) - library, supports many
(use-package! llm
  :defer t
  :config
  (setq! llm-warn-on-nonfree nil)
  (require 'llm-openai)
  (require 'llm-gemini)
  (defconst akn/llms-alist
    `(("gpt-4o-mini" . ,(make-llm-openai :key (akn/chatgpt-api-key) :chat-model "gpt-4o-mini"))
      ("gpt-4o" . ,(make-llm-openai :key (akn/chatgpt-api-key) :chat-model "gpt-4o"))
      ("gemini-2.0-flash" . ,(make-llm-gemini :key (akn/gemini-api-key) :chat-model "gemini-1.5-flash"))
      ("gemini-2.0-flash-thinking" . ,(make-llm-gemini :key (akn/gemini-api-key) :chat-model "gemini-1.5-flash-thinking"))
      ("gemini-2.0-pro" . ,(make-llm-gemini :key (akn/gemini-api-key) :chat-model "gemini-1.5-flash-thinking"))))
  (defconst akn/llms
    (mapcar #'cdr akn/llms-alist))
  (defconst akn/llm-main
    (car akn/llms)))

;;; semext (https://github.com/ahyatt/semext) -  Semantic versions of existing Emacs functionality

(use-package! semext
  :defer t
  :commands (semext-forward-part semext-backward-part semext-clear-cache semext-query-replace semext-search-forward semext-search-backward)
  :config
  (require 'llm)
  (setq! semext-provider
         (akn/->> akn/llms
                  (seq-filter (lambda (provider) (member 'json-response (llm-capabilities provider))))
                  (seq-first))))

;;; magit-gptcommit

(use-package! magit-gptcommit
  :defer t
  :commands (magit-gptcommit-status-buffer-setup magit-gptcommit-generate magit-gptcommit-abort magit-gptcommit-remove-section magit-gptcommit-commit-accept magit-gptcommit-commit-create magit-gptcommit-commit-quick)
  :init
  (map! :after git-commit
        :map git-commit-mode-map
        :localleader
        "g" #'magit-gptcommit-generate
        "a" #'magit-gptcommit-commit-accept)

  ;; from `magit-gptcommit-status-buffer-setup'
  (after! magit-commit
    (transient-append-suffix #'magit-commit '(1 -1)
      ["GPT Commit"
       :if magit-anything-staged-p
       ("G" "Generate" (lambda () (interactive) (call-interactively #'magit-gptcommit-generate)))
       ("Q" "Quick Accept" (lambda () (interactive) (call-interactively #'magit-gptcommit-commit-quick)))
       ("C" "Accept" (lambda () (interactive) (call-interactively #'magit-gptcommit-commit-create)))]))
  :config
  (require 'llm)
  (setq! magit-gptcommit-llm-provider akn/llm-main))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

;;; chat (https://github.com/iwahbe/chat.el)
(use-package! chat
  :defer t
  :config
  (setq! chat-model "gpt-4o-mini")
  (defadvice! akn/chat-get-api-key-a ()
    :override #'chat-get-api-key
    (or chat-api-key
        (and chat-api-env-key (getenv chat-api-env-key))
        ;; ADDED
        (akn/chatgpt-api-key)
        (user-error
         "Could not get OpenAI API key.  Either set `chat-api-key' or ensure that \"%s\" is set"
         chat-api-env-key))))

;;; GitHub Copilot (https://github.com/copilot-emacs/copilot.el)
;; accept completion from copilot and fallback to company
(use-package! copilot
  :defer t
  ;; :defer-incrementally (dash s editorconfig)
  :init
  (defvar akn/copilot-disabled-modes '(metal-mercury-mode mercury-mode racket-mode))
  (defun akn/copilot-mode-when-ready ()
    (unless (member major-mode akn/copilot-disabled-modes)
      (after! copilot
        (copilot-mode 1))))
  ;; (add-hook! 'prog-mode-hook #'akn/copilot-mode-when-ready)
  (defun akn/disable-copilot ()
    (interactive)
    (when (fboundp 'copilot-mode)
      (copilot-mode -1)))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq! copilot-indent-offset-warning-disable t))
