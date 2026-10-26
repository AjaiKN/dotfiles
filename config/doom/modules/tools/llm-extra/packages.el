;;; tools/llm-extra/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! copilot :pin "56c043ae5078c4f8e13538c45f2a4227c9c1eb00"
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! gptel :pin "8329ee709ebf91d59a07dc13f193f173118b1ae2")
(package! evedel :pin "d979801f5f496ff20aebf4c3343bffcd0e0d3a0b")

(package! ellama :pin "b6e71f41583e85d2e1fe93ccaa34593f7b4015b8")

(package! llm :pin "38cfcf96b55e5a042862a099661ad706b60755d0")

(package! magit-gptcommit :pin "b3ac0bfad8b06b6930dc4e35e3adefb4b6646193")

(package! chatgpt-shell :pin "83b327ba55cd116d892a9b44f00bc65543b7c22e" :disable t)
(package! shell-maker :pin "e089f07fb4f5a8ce226932e192a2c11682a51102")
(package! acp :pin "2959da5fa8657da17a78ac6ceef303a50e3e69de")
(package! agent-shell :pin "642dfe2257332d55d5ea258d0d07812c3d571518")
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
(package! aidermacs :pin "03ec5747b87920678d0b0ad8bc798ec0e5923125")

(package! semext :pin "6d05e243d066c2f8b3cd44081ea31cb1c445e535"
  :recipe (:host github :repo "ahyatt/semext"))
