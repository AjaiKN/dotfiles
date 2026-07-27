;;; tools/llm-extra/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(package! copilot :pin "277ca357422ba34bcf7fe650cb720580994eea84"
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! gptel :pin "8329ee709ebf91d59a07dc13f193f173118b1ae2")
(package! evedel :pin "d979801f5f496ff20aebf4c3343bffcd0e0d3a0b")

(package! ellama :pin "2857f85b8f10eb587afb8cb3da1d96cb236b2856")

(package! llm :pin "06f8057c108063df8661dbf0f5e4e515ae1fdb78")

(package! magit-gptcommit :pin "97dfcf33777731ba8d8ad0522c2deb0554a143fe")

(package! chatgpt-shell :pin "83b327ba55cd116d892a9b44f00bc65543b7c22e" :disable t)
(package! shell-maker :pin "071c6df3ca22a2f4c0daa689848ac9bd21bf4e2b")
(package! acp :pin "a29cb161ac95f1819f34481a98666707661c5cf8")
(package! agent-shell :pin "1780e0b2501aa172c559c24d76024b8802e8d8de")
(package! agent-recall :pin "166f421bce4a6550507e0bd2b896956d0d9d4a94")
(package! agent-shell-bookmark :pin "c1eab34bff4f35bf929885ed5045c6100afcf496"
  :recipe (:host github :repo "dcluna/agent-shell-bookmark"))
(package! agent-shell-macext :pin "1f9b4e0cb1fb180f6246ed0c99a72f55187066dd"
  :recipe (:host github :repo "cxa/agent-shell-macext"))

(package! claude-code-ide :pin "1de17bbadc650962a05fd68463fdff71697ec649"
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

(package! chat :pin "a14df12bda3951e53553426629f4af7a638f6eee" :disable t
  :recipe (:host github :repo "iwahbe/chat.el"))

;; aider.el (https://github.com/tninja/aider.el) vs aidermacs (https://github.com/MatthewZMD/aidermacs):
;; - https://github.com/MatthewZMD/aidermacs/tree/0c88c2f12d1278b3753235d019bfbbb28413fa03?tab=readme-ov-file#aidermacs-vs-aiderel
;; - https://old.reddit.com/r/emacs/comments/1in88k6/aidermacs_aider_ai_pair_programming_in_emacs/
;; - https://old.reddit.com/r/emacs/comments/1j5j1s9/aidermacs_in_action_emacs_ai_pair_programming_w/
;; (package! aider)
(package! aidermacs :pin "1158314ef4319fd7f0c0eb84a1d17250a6b5648f")

(package! semext :pin "6d05e243d066c2f8b3cd44081ea31cb1c445e535"
  :recipe (:host github :repo "ahyatt/semext"))
