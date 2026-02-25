java-base-mode java-mode java-ts-mode
:when (and (not (doom-point-in-comment-p))
           (if (fboundp 'evil-eolp) (evil-eolp) (eolp)))

(pkg "package " q ";")
(imp "import " q ";")

(fileclassname (p (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name) "Name")))))

(class     "class "     (i fileclassname) " {" n> r> n> "}" > n)
(interface "interface " (i fileclassname) " {" n> r> n> "}" > n)
(enum      "enum "      (i fileclassname) " {" n> (r> "OPTION_ONE,") n> "}" > n)
(record    "record "    (i fileclassname) "("(p "int x")") {"q"}")

(getter (p "int") " get"(capitalize (car (string-split prop " ")))"() {" n>
        "return this."(p "prop" prop)";" > n>
        "}" > %)
(main "public static void main(String[] args) {" n> q n> "}" > %)
(psvm (i main))

(for & "for ("p") {" n> r> n> "}" > %)
(fori & "for ("(p"int i = 0")"; "(p"i < N") "; "(p"i++")") {" n>
      r> n>
      "}" > %)
(foreach & "for ("(p"Object o")" : "(p"iterator")" {" n>
         r> n>
         "}" > %)
(while & "while ("(p "true")") {" n>
       r> n>
       "}" > %)
(if "if ("(p "true")") {" n> r> n> "}" > %)
(elif "else if ("(p "true")") {" n> r> n> "}" > %)
(else "else "p"{" n> r> n> "}" >)
(ife & "if ("(p "true")") {" n> p n> "} else {" > n> p n> "}" > %)
(switch "switch ("(p "val")") {" n> p n> "}" > %)

(try "try {" n>
     R> n>
     "} catch ("(p "Throwable")" e) {" > n>
     p n>
     "}" > %)

(return "return " r ";" %)

java-base-mode java-mode java-ts-mode
:when (doom-point-in-comment-p)

(param "@param " (p "parameter") " " r)
(return "@return " (r "description"))
