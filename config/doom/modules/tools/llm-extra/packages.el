;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! copilot :pin "4f51b3c21c42756d09ee17011201ea7d6e18ff69"
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! gptel :pin "8329ee709ebf91d59a07dc13f193f173118b1ae2")
(package! evedel :pin "d979801f5f496ff20aebf4c3343bffcd0e0d3a0b")

(package! ellama :pin "018c2151d1f231a3e3a15a5e99cf39157d758ee4")

(package! llm :pin "5ce1d6bd1359a3204a65e02d5035e05205c6d411")

(package! magit-gptcommit :pin "97dfcf33777731ba8d8ad0522c2deb0554a143fe")

(package! chatgpt-shell :pin "cdde960ce5e20108e60e38e394b1a658905780b8")

(package! chat :pin "a14df12bda3951e53553426629f4af7a638f6eee" :disable t
  :recipe (:host github :repo "iwahbe/chat.el"))

;; aider.el (https://github.com/tninja/aider.el) vs aidermacs (https://github.com/MatthewZMD/aidermacs):
;; - https://github.com/MatthewZMD/aidermacs/tree/0c88c2f12d1278b3753235d019bfbbb28413fa03?tab=readme-ov-file#aidermacs-vs-aiderel
;; - https://old.reddit.com/r/emacs/comments/1in88k6/aidermacs_aider_ai_pair_programming_in_emacs/
;; - https://old.reddit.com/r/emacs/comments/1j5j1s9/aidermacs_in_action_emacs_ai_pair_programming_w/
;; (package! aider)
(package! aidermacs :pin "d944f430eae936de2a9aa8121c7eaca2ca7be948")

(package! semext :pin "6d05e243d066c2f8b3cd44081ea31cb1c445e535"
  :recipe (:host github :repo "ahyatt/semext"))
