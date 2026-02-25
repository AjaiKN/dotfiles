java-base-mode java-mode java-ts-mode
:when (and (not (doom-point-in-comment-p))
           (if (fboundp 'evil-eolp) (evil-eolp) (eolp)))

(class "class "
       (p (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name) ""))))
       " {" n>
       r> n>
       "}" > n)
(interface "interface "
           (p (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name) ""))))
           " {" n>
           r> n>
           "}" > n)
(record "record "
        (p (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name) ""))))
        " ("(p "int x")") {"p"}")

(getter (p "int") " get"(capitalize (car (string-split prop " ")))"() {" n>
        "return this."(p "prop" prop)";" > n>
        "}" > %)

(for & "for ("(p"int i = 0")"; "(p"i < N") "; "(p"i++")") {" n>
     r> n>
     "}" > %)
(foreach & "for ("(p"Object o")" : "(p"iterator")" {" n>
         r> n>
         "}" > %)
(while & "while ("(p "true")") {" n>
       r> n>
       "}" > %)
(if & "if ("(p "true")") {" n>
    r> n>
    "}" > %)
(ife & "if ("(p "true")") {" n>
     p n>
     "} else {" > n>
     p n>
     "}" > %)

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
