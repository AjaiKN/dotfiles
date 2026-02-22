;;; editor/tempel/+test.el -*- lexical-binding: t; -*-

(ert-deftest +tempel--parse-yasnippet-template ()
  (should (equal (+tempel--parse-yasnippet-template "") nil))
  (should (equal (+tempel--parse-yasnippet-template "hi") '("hi")))
  (should (equal (+tempel--parse-yasnippet-template "$1") '((yas-field 1))))
  (should (equal (+tempel--parse-yasnippet-template "ab$1") '("ab" (yas-field 1))))
  (should (equal (+tempel--parse-yasnippet-template "${23}") '((yas-field 23))))
  (should (equal (+tempel--parse-yasnippet-template "abc${23}def") '("abc" (yas-field 23) "def")))
  (should (equal (+tempel--parse-yasnippet-template "${23:default value}") '((yas-field 23 ("default value") nil))))
  (should (equal (+tempel--parse-yasnippet-template "`yas-selected-text`") '((yas-subst yas-selected-text))))
  (should (equal (+tempel--parse-yasnippet-template "`(concat \"a b\" \"c\")`") '((yas-subst (concat "a b" "c")))))
  (should (equal (+tempel--parse-yasnippet-template "${23}") '((yas-field 23))))
  (should (equal (+tempel--parse-yasnippet-template "${2:$(upcase yas-text)}") '((yas-mirror 2 (upcase yas-text)))))
  (should (equal (+tempel--parse-yasnippet-template-1 "hello$(upcase yas-text)" 'nested)
                 '(("hello") (upcase yas-text))))
  (should (equal (+tempel--parse-yasnippet-template "${1:hello$(upcase yas-text)}")
                 '((yas-field 1 ("hello") (upcase yas-text)))))
  (should (equal (+tempel--parse-yasnippet-template "${1:$$(upcase yas-text)}")
                 '((yas-field 1 () (upcase yas-text)))))
  (should (equal (+tempel--parse-yasnippet-template
                  "<p>`(when yas-prefix \"\\n\")`$0`(when yas-prefix \"\\n\")`</p>")
                 '("<p>"
                   (yas-subst (when yas-prefix "\n"))
                   (yas-field 0)
                   (yas-subst (when yas-prefix "\n"))
                   "</p>")))
  (should (equal (+tempel--parse-yasnippet-template
                  "for ($1;$2;$3) {
  `yas-selected-text`$0
}")
                 '("for (" (yas-field 1) ";" (yas-field 2) ";" (yas-field 3) ") {\n  "
                   (yas-subst yas-selected-text) (yas-field 0)
                   "\n}")))
  (should (equal (+tempel--parse-yasnippet-template
                  "\\section{${1:\"Titel der Tour\"}}%
\\index{$1}%
\\label{${2:\"waiting for reftex-label call...\"$(unless yas-modified-p (reftex-label nil 'dont-insert))}}%")
                 '("\\section{" (yas-field 1 ("\"Titel der Tour\"") nil)
                   "}%\n\\index{" (yas-field 1)
                   "}%\n\\label{" (yas-field 2 ("\"waiting for reftex-label call...\"") (unless yas-modified-p (reftex-label nil 'dont-insert))) "}%")))
  (should (equal (+tempel--parse-yasnippet-template "<div$1>\n $0\n </div>")
                 '("<div" (yas-field 1) ">\n " (yas-field 0) "\n </div>")))
  (should (equal (+tempel--parse-yasnippet-template "<div${1: id=\"${2:some_id}\"}>$0</div>")
                 '("<div" (yas-field 1 (" id=\"" (yas-field 2 ("some_id") nil) "\"") nil) ">" (yas-field 0) "</div>")))
  (should (equal (+tempel--parse-yasnippet-template "abc def $>")
                 '("abc def " yas-indent))))

(save-selected-window
  (ert t))
