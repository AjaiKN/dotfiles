;;; editor/things/config.el -*- lexical-binding: t; -*-

;; This module is very much a WIP

(require 'akn)

(use-package! things
  :defer t)

;; from https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/awaken.org#things
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

  ;; (things-evil-define aggregated-comment things-aggregated-comment :keys "c")

  (things-evil-define string things-string :keys "S")

  (things-evil-define line things-line :keys "L")

  (things-evil-define paragraph paragraph :keys "p")

  (put 'akn-defun 'things-get-a
       (pcase-lambda (`(,_thing . (,beg . ,end)))
         (save-excursion
           (goto-char beg)
           (things-bounds 'akn-defun-comments `(,beg . ,end)))))
  ;; (things-evil-define function things-function :keys "f")
  (things-evil-define function akn-defun :keys "f")

  (things-define-pair 'things-paren "(" ")")
  (things-evil-define paren things-paren :keys '("(" ")"))

  (things-define-pair 'things-bracket "[" "]")
  (things-evil-define bracket things-bracket :keys '("[" "]"))

  (things-define-pair 'things-curly "{" "}")
  (things-evil-define curly things-curly :keys '("{" "}"))

  (things-define-pair 'things-angle "<" ">")
  (things-evil-define angle things-angle :keys '("<" ">"))

  (things-define-separator 'things-comma ",")
  (things-evil-define comma things-comma :keys ",")

  ;; TODO: only single character pairs are currently supported
  ;; (things-define-pair 'things-backslash-bracket "\\[" "\\]")
  (things-evil-define anyblock (things-paren
                                things-curly
                                ;; things-backslash-bracket
                                things-bracket
                                things-angle)
                      :keys "d"))

;;; Listing things
;; Helpers to see what all the things are

(defun +things-all-symbols ()
  (seq-uniq
   (cl-loop for sym being the symbols
            if (or (get sym 'forward-op))
            collect sym
            else if (and (functionp sym)
                         (string-match (rx bos "forward-" (group (+ (not "@"))) eos)
                                       (symbol-name sym))
                         (not (string-match-p (rx bos (or "forward-to-"
                                                          (: "forward-thing" eow)))
                                              (symbol-name sym))))
            collect (intern (match-string-no-properties 1 (symbol-name sym))))))

(defun +things-list ()
  (cl-loop for thing in (+things-all-symbols)
           collect
           (append (list thing
                         (cons 'forward-op
                               (or (get thing 'forward-op)
                                   (akn/symbol-format "forward-%s" thing))))
                   (cl-loop for prop in '(beginning-op
                                          end-op
                                          bounds-of-thing-at-point
                                          thing-at-point)
                            if (get thing prop)
                            collect (cons prop (get thing prop)))
                   (cl-loop for (key val) on (symbol-plist thing) by #'cddr
                            if (string-match-p (rx bos "things-") (symbol-name key))
                            collect (cons key val)))))

(defun +things-list-props-only ()
  (cl-loop for (key . val) in (+things-list)
           collect (cons key (delq 'forward-op (mapcar #'car val)))))

;; (setq +things-all-symbols (+things-all-symbols))
;; (setq +things-list (+things-list))
;; (setq +things-props (+things-list-props-only))
