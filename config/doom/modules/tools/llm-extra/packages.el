;;; tools/llm-extra/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! copilot :pin "90f429b100418d897b673d6c36c2817364b28e3e"
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! gptel :pin "8329ee709ebf91d59a07dc13f193f173118b1ae2")
(package! evedel :pin "d979801f5f496ff20aebf4c3343bffcd0e0d3a0b")

(package! ellama :pin "a86e9154ae88907d3de2710195810cda986ef0d8")

(package! llm :pin "9aaeaee98fc0e506ff3a3188dfb36b9c612ca240")

(package! magit-gptcommit :pin "b3ac0bfad8b06b6930dc4e35e3adefb4b6646193")

(package! chatgpt-shell :pin "83b327ba55cd116d892a9b44f00bc65543b7c22e" :disable t)
(package! shell-maker :pin "f448a74a8eded23aa42f8d60a41c5d8d3a183d07")
(package! acp :pin "242cef63d76cc1073485847f67a21f6d8406d158")
(package! agent-shell :pin "e78b43487007d59415d34c3cec4fadf814b8a373")
(package! agent-recall :pin "67651796756668479ff954b3ea6f7fb7312762f3")
(package! agent-shell-bookmark :pin "c1eab34bff4f35bf929885ed5045c6100afcf496"
  :recipe (:host github :repo "dcluna/agent-shell-bookmark"))
(package! agent-shell-macext :pin "41e0a7d31434a0f3fe08c83d9acc45b5402bd3b7"
  :recipe (:host github :repo "cxa/agent-shell-macext"))

(package! claude-code-ide :pin "50a3d55262805d7207889ed429ff30da96fbf68b"
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

(package! chat :pin "a14df12bda3951e53553426629f4af7a638f6eee" :disable t
  :recipe (:host github :repo "iwahbe/chat.el"))

;; aider.el (https://github.com/tninja/aider.el) vs aidermacs (https://github.com/MatthewZMD/aidermacs):
;; - https://github.com/MatthewZMD/aidermacs/tree/0c88c2f12d1278b3753235d019bfbbb28413fa03?tab=readme-ov-file#aidermacs-vs-aiderel
;; - https://old.reddit.com/r/emacs/comments/1in88k6/aidermacs_aider_ai_pair_programming_in_emacs/
;; - https://old.reddit.com/r/emacs/comments/1j5j1s9/aidermacs_in_action_emacs_ai_pair_programming_w/
;; (package! aider)
(package! aidermacs :pin "b9a2512e54d8366a0b0472c418d1d2610c98c3ae")

(package! semext :pin "6d05e243d066c2f8b3cd44081ea31cb1c445e535"
  :recipe (:host github :repo "ahyatt/semext"))
