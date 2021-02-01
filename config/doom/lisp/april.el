;;; april.el --- April language minor mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A minor mode and input methods for April (https://github.com/phantomics/april),
;;  an APL subset embedded into Common Lisp.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'cape-char)
(require 'akn)


;; https://github.com/phantomics/april?tab=readme-ov-file#enabling-apl-input-in-emacs
;; https://github.com/justin2004/vim-apl/blob/master/keymap/dyalog.vim
;; https://github.com/jthing/apl-mode/blob/main/jpt-apl-mode.el
;; https://github.com/harsman/dyalog-mode/blob/master/dyalog-mode.el
;; https://github.com/lokedhs/gnu-apl-mode/blob/master/gnu-apl-symbols.el
;; https://github.com/lokedhs/gnu-apl-mode/blob/master/gnu-apl-input.el

(defvar april--gnu-apl--symbols
  '(;; Top row
    ("diamond" "◊" "``")
    ("diaeresis" "¨" "1") ("i-beam" "⌶" "!")
    ("macron" "¯" "2") ("del-tilde" "⍫" "@")
    ("less-than" "<" "3") ("del-stile" "⍒" "#")
    ("less-than-or-equal-to" "≤" "4") ("delta-stile" "⍋" "$")
    ("equals" "=" "5") ("circle-stile" "⌽" "%")
    ("greater-than-or-equal-to" "≥" "6") ("circle-backslash" "⍉" "^")
    ("greater-than" ">" "7") ("circled-minus" "⊖" "&")
    ("not-equal-to" "≠" "8") ("circle-star" "⍟" "*")
    ("logical-or" "∨" "9") ("down-caret-tilde" "⍱" "(")
    ("logical-and" "∧" "0") ("up-caret-tilde" "⍲" ")")
    ("multiplication-sign" "×" "-") ("exclamation-mark" "!" "_")
    ("division-sign" "÷" "=") ("quad-divide" "⌹" "+")

    ;; First row
    ("question-mark" "?" "q")
    ("omega" "⍵" "w") ("omega-underbar" "⍹" "W")
    ("epsilon" "∊" "e") ("epsilon-underbar" "⍷" "E")
    ("rho" "⍴" "r")
    ("tilde" "∼" "t") ("tilde-diaeresis" "⍨" "T")
    ("uparrow" "↑" "y") ("yen-sign" "¥" "Y")
    ("downarrow" "↓" "u")
    ("iota" "⍳" "i") ("iota-underbar" "⍸" "I")
    ("circle" "○" "o") ("circle-diaeresis" "⍥" "O")
    ("star-operator" "⋆" "p") ("star-diaeresis" "⍣" "P")
    ("leftarrow" "←" "[") ("quote-quad" "⍞" "{")
    ("rightarrow" "→" "]") ("zilde" "⍬" "}")
    ("right-tack" "⊢" "\\") ("left-tack" "⊣" "|")

    ;; Second row
    ("alpha" "⍺" "a") ("alpha-underbar" "⍶" "A")
    ("left-ceiling" "⌈" "s")
    ("left-floor" "⌊" "d")
    ("underscore" "_" "f") ("del-tilde" "⍫" "F")
    ("nabla" "∇" "g")
    ("increment" "∆" "h") ("delta-underbar" "⍙" "H")
    ("ring-operator" "∘" "j") ("jot-diaeresis" "⍤" "J")
    ("apostrophe" "'" "k") ("quad-diamond" "⌺" "K")
    ("quad" "⎕" "l") ("squish-quad" "⌷" "L")
    ("down-tack-jot" "⍎" ";") ("identical-to" "≡" ":")
    ("up-tack-jot" "⍕" "'") ("not-identical-to" "≢" "\"")

    ;; Third row
    ("subset-of" "⊂" "z")
    ("superset-of" "⊃" "x") ("greek-letter-chi" "χ" "X")
    ("intersection" "∩" "c") ("left-shoe-stile" "⍧" "C")
    ("union" "∪" "v")
    ("up-tack" "⊥" "b") ("pound-sign" "£" "B")
    ("down-tack" "⊤" "n")
    ("divides" "|" "m")
    ("shoe-jot" "⍝" ",") ("comma-bar" "⍪" "<")
    ("backslash-bar" "⍀" ">")
    ("slash-bar" "⌿" "/") ("quad-colon" "⍠" "?")

    ;; Extras
    ("pi" "π") ("root" "√") ("inverted-exclamation-mark" "¡") ("quad-backslash" "⍂") ("inverted-question-mark" "¿")))

(defvar april--jpt-apl-data
  '((?← "assign" ?< ?-) (?→ "branch" ?- ?>)
    (?+ "conjugate/plus") (?- "negate/minus")
    (?× "direction/times" ?x ?x) (?÷ "reciprocal/divide" ?: ?-)
    (?⍟ "ln/log" ?* ?o) (?! "factorial/binominal") (?\? "roll/deal")
    (?⌹ "matrix inverse/matrix divide" ?\[ ?-) (?○ "pi times/circular" ?O ?O)
    (?| "magnitude/reidue") (?⌈ "ceiling/maximum" ?7 ?7) (?⌊ "floor/minimum" ?l ?l) (?⊥ "decode" ?| ?_)
    (?⊤ "encode" ?T ?T) (?⊣ "same/left" ?- ?|) (?⊢ "same/right" ?| ?-)
    (?= "equal") (?≠ "unique mask/not equal" ?= ?/) (?≤ "less or equal to" ?< ?=) (?≥ "greater or equal to" ?> ?=)
    (?≡ "depht/match" ?= ?=) (?≢"tally/not match" ?7 ?=)
    (?∨ "Greatest common divisor/or" ?v ?v) (?∧ "lowest common multiple/and" ?^ ?^) (?⍱ "nand" ?v ?~) (?⍲ "nor" ?^ ?~)
    (?↑ "mix/take" ?^ ?|) (?↓ "split/drop" ?v ?|) (?⊂ "enclose/partial enclose" ?\( ?\() (?⊃ "first/pick" ?\) ?\))
    (?⊆ "nest/partition" ?\( ?_) (?⌷ "index" ?| ?\]) (?⍋ "grade up" ?A ?|) (?⍒ "grade down" ?V ?|)
    (?⍳ "indices/indices of" ?i ?i) (?⍸ "where/where index" ?i ?_) (?∊ "enlist/member of" ?e ?e) (?⍷ "find" ?e ?_)
    (?∪ "unique/union" ?u ?u) (?∩ "intersection" ?n ?n) (?~ "not/without")
    (?/ "replicate/reduce") (?\\ "expand/scan")
    (?⌿ "replicate first/reduce first" ?/ ?-) (?⍀ "expand first/scan first" ?\\ ?-)
    (?, "ravel/catinate/laminate") (?⍪ "table/catenate first" ?, ?-) (?⍴ "shape/reshape" ?p ?p) (?⌽ "reverse/rotate" ?O ?|)
    (?⊖ "reverse first/rotate first" ?O ?-) (?⍉ "transpose/reorder axis" ?O ?\\)
    (?¨ "each" ?: ?:) (?⍨ "constant/self/swap" ?~ ?:) (?⍣ "repeat" ?* ?:) (?. "outer product (.∘)/inner product")
    (?∘ ".∘ outer product & ∘ curry/compose" ?o ?o) (?⍤ "rank/atop" ?o ?:) (?ö "over" ?O ?:)
    (?@ "at") (?⍞ "Character input/output" ?\[ ?\')
    (?⎕ "systen name " ?\[ ?\]) (?⍠ "variant" ?\[ ?:) (?⌸ "index key/key" ?\[ ?=) (?⌺ "stencil" ?\[ ?<) (?⌶ "I-beam" ?T ?_)
    (?◊ "statement seperator" ?< ?>) (?⍝ "comment" ?o ?n)
    (?⍵ "right argument/right operand" ?w ?w) (?⍹ "values as OP" ?w ?_) (?⍺ "left argument/left operand" ?a ?a) (?⍶ "values as OP" ?a ?_)
    (?∇ "recursion" ?v ?-) (?& "spawn")
    (?¯ "negative" ?- ?-) (?⍬ "empty numeric vector" ?0 ?~)))

(defvar april--jpt-apl-exclude-plist
    (list
     :ascii  '(?+ ?- ?! ?\? ?= ?~ ?| ?/ ?\\ ?, ?. ?@ ?&)
     :dyalog '(?⍞ ?⌶ ?&)
     :gnu    '(?⍹ ?⍶)))

(quail-define-package
 "april1" "April1" "April1")
(dolist (l april--gnu-apl--symbols)
  (let ((name (car l))
        (symbol (cadr l))
        (chars (caddr l)))
    (quail-defrule (format "`%s" chars) symbol)
    (when name
      (quail-defrule (format "``%s" name) symbol))))

(quail-define-package
 "april2" "April2" "April2")
(dolist (l april--jpt-apl-data)
  (cl-destructuring-bind (symbol name . chars) l
    (when name
      (quail-defrule (format "``%s" name) symbol nil 'append))))

(quail-define-package
 "april" "April" "April")
(dolist (l april--gnu-apl--symbols)
  (let ((name (car l))
        (symbol (cadr l))
        (chars (caddr l)))
    (quail-defrule (format "`%s" chars) symbol)
    (when name
      (quail-defrule (format "``%s" name) symbol))))
(dolist (l april--jpt-apl-data)
  (cl-destructuring-bind (symbol name . chars) l
    (when name
      (quail-defrule (format "``%s" name) symbol nil 'append))))

;;;###autoload (autoload 'cape-april1 "cape-april1" nil t)
(cape-char--define april1 "april1" ?\`)
(defvar cape-april2-prefix-required t)
;;;###autoload (autoload 'cape-april2 "cape-april2" nil t)
(cape-char--define april2 "april2" ?\` ?\`)

(defvar april--buffer-to-reset nil)
;;;###autoload
(defvar-keymap april-mode-map
  "`" (akn/cmds!
       t ;(nth 3 (syntax-ppss))
       (akn/cmds! (looking-back (rx "``") 3)
                  (akn/defun april--cape3 ()
                    (interactive)
                    (delete-backward-char 2)
                    (insert "◊"))
                  (eq (char-before) ?\`)
                  (akn/defun april--cape2 ()
                    (interactive)
                    (insert "`")
                    (let ((buf (current-buffer))
                          (original corfu-preselect))
                      (setq-local corfu-preselect 'first
                                  corfu-on-exact-match 'insert)
                      (setq april--buffer-to-reset (current-buffer))
                      (cape-april2 t)
                      (add-transient-hook! 'completion-in-region-mode-hook
                        (with-current-buffer april--buffer-to-reset
                          (kill-local-variable 'corfu-preselect)
                          (kill-local-variable 'corfu-on-exact-match)))))
                  (akn/defun april--cape1 ()
                    (interactive)
                    (let ((buf (current-buffer))
                          (original corfu-preselect))
                      (setq-local corfu-preselect 'first
                                  corfu-on-exact-match 'insert)
                      (setq april--buffer-to-reset (current-buffer))
                      (cape-april1 t)
                      (add-transient-hook! 'completion-in-region-mode-hook
                        (with-current-buffer april--buffer-to-reset
                          (kill-local-variable 'corfu-preselect)
                          (kill-local-variable 'corfu-on-exact-match))))))))
(define-minor-mode april-mode
  ""
  :global nil)



(provide 'april)
;;; april.el ends here
