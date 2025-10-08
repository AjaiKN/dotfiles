;;; editor/things/config.el -*- lexical-binding: t; -*-

;; from https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/awaken.org#things

(use-package! things
  :defer t)

(use-package! things-evil
  :when (modulep! :editor evil)
  :after-call doom-first-buffer-hook
  :config
  (setq! things-evil-inner-key "i"
         things-evil-a-key "a"
         things-evil-inside-key nil
         things-evil-around-key nil

         things-evil-next-key "n"
         things-evil-last-key "l"
         things-evil-remote-key "r")

  ;; (general-def '(visual operator) "RET" #'things-evil-last-text-object)

  (general-def '(inner outer) "l" nil)
  (general-def '(inner outer) "n" nil)
  (general-def '(inner outer) "r" nil)
  ;; temporary
  ;; (general-def '(visual operator) "I" nil "A" nil)
  ;; keep I and A with visual block selection
  ;; (general-def 'visual 'override
  ;;   :predicate '(eq (evil-visual-type) 'block)
  ;;   "I" #'evil-insert
  ;;   "A" #'evil-append)

  (things-evil-define aggregated-comment things-aggregated-comment :keys "c")

  (things-evil-define string things-string :keys "S")

  (evil-define-text-object evil-a-buffer (count &optional _beg _end _type)
    "Select the entire buffer."
    (evil-range (point-min) (point-max)))

  (general-def '(inner outer) "e" #'evil-a-buffer)

  (things-evil-define line things-line :keys "L")

  (things-evil-define paragraph paragraph :keys "p")

  (things-evil-define function things-function :keys "f")

  (things-define-pair 'things-paren "(" ")")
  (things-evil-define paren things-paren :keys "(")

  (things-define-pair 'things-bracket "[" "]")
  (things-evil-define bracket things-bracket :keys "[")

  (things-define-pair 'things-curly "{" "}")
  (things-evil-define curly things-curly :keys "{")

  (things-define-pair 'things-angle "<" ">")
  (things-evil-define angle things-angle :keys "<")

  (things-define-separator 'things-comma ",")
  (things-evil-define comma things-comma :keys ",")

  (things-evil-define anyblock (things-paren
                                things-bracket
                                things-angle
                                things-curly)
                      :keys "d"))
