;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! copilot :pin "8e43edf1f3efe094ea42a21863b3b742a339742c"
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! gptel :pin "8329ee709ebf91d59a07dc13f193f173118b1ae2")
(package! evedel :pin "d979801f5f496ff20aebf4c3343bffcd0e0d3a0b")

(package! ellama :pin "2857f85b8f10eb587afb8cb3da1d96cb236b2856")

(package! llm :pin "06f8057c108063df8661dbf0f5e4e515ae1fdb78")

(package! magit-gptcommit :pin "97dfcf33777731ba8d8ad0522c2deb0554a143fe")

(package! chatgpt-shell :pin "83b327ba55cd116d892a9b44f00bc65543b7c22e")

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
