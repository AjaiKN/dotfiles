;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! copilot :pin "f0e4ce61ba7565f9eb9393e88bd88868872a7d4f"
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! gptel :pin "8329ee709ebf91d59a07dc13f193f173118b1ae2")
(package! evedel :pin "d979801f5f496ff20aebf4c3343bffcd0e0d3a0b")

(package! ellama :pin "3b8cb569409ccbef7e9e955aefcd550c4be3e607")

(package! llm :pin "8582703fd78e17db86d8e6c84663e0df871a3575")

(package! magit-gptcommit :pin "41022230c565c52a7db6bda8ceacdf064456e1a4")

(package! chatgpt-shell :pin "5fc6c333b3bc91fba41871f5a89217804a66eabd")

(package! chat :pin "a14df12bda3951e53553426629f4af7a638f6eee" :disable t
  :recipe (:host github :repo "iwahbe/chat.el"))

;; aider.el (https://github.com/tninja/aider.el) vs aidermacs (https://github.com/MatthewZMD/aidermacs):
;; - https://github.com/MatthewZMD/aidermacs/tree/0c88c2f12d1278b3753235d019bfbbb28413fa03?tab=readme-ov-file#aidermacs-vs-aiderel
;; - https://old.reddit.com/r/emacs/comments/1in88k6/aidermacs_aider_ai_pair_programming_in_emacs/
;; - https://old.reddit.com/r/emacs/comments/1j5j1s9/aidermacs_in_action_emacs_ai_pair_programming_w/
;; (package! aider)
(package! aidermacs :pin "658eca5aaf63ab0f2ec732dc0b87f64f9e88a05d")

(package! semext :pin "10b2c82d7c1148e4b836324907995f5179483c18"
  :recipe (:host github :repo "ahyatt/semext"))
