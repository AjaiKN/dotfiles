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
(fori & "for ("(p"int i = 0")"; "(p"i < N") "; "(p"i++")") {" n> r> n> "}" > %)
(foreach & "for ("(p"Object o")" : "(p"iterator")" {" n> r> n> "}" > %)
(iter (i foreach))
(while & "while ("(p "true")") {" n> r> n> "}" > %)
(dowhile & "do {" n> p n> "} while ("(p "true")");" > %)
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
(tryresources & "try ("") {" n> r> n> "}" > %)
(finally "finally {" n> r> n> "}" > %)

(return "return " r ";" %)

(currentclassname
 (p (or (when (require 'smartparens nil 'noerror)
          (save-excursion
            (cl-loop until (eq (point) (progn (sp-up-sexp) (point)))
                     thereis
                     (save-excursion
                       (sp-backward-sexp)
                       (goto-char (pos-bol))
                       (and (search-forward-regexp (rx "class" (+ space) (group (+ alnum)) (* space) "{")
                                                   (pos-eol)
                                                   'noerror)
                            (match-string-no-properties 1))))))
        (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name) "Name"))))))

(constructor "public " (i currentclassname) "("p") {" n> r n "}" >)
(ctor (i constructor))

(doc "/**" n> "* " r > n> "*/" > %)

(test "@Test" n> "public void "p"() {" n> r> n> "}" > %)
(testclass (indent
            "import junit.framework.*;
             import junit.textui.*;

             public class "
            (let ((c (s-upper-camel-case (file-name-base (or (buffer-file-name) (buffer-name) "Thing")))))
              (if (string-prefix-p "Test" c) c (concat "Test" c)))
            " extends TestCase {
               protected void setUp() {
                 "p"
               }

               "(i test)"
             }"))

java-base-mode java-mode java-ts-mode
:when (not (doom-point-in-comment-p))

(override & "@Override" n>)

(sysout "System.out.println("r");")
(syserr "System.err.println("r");")
(sout (i sysout))
(serr (i syserr))
(println (i sysout))
(print "System.out.print("r");")
(printf "System.out.printf(\""p"\", "r");")

java-base-mode java-mode java-ts-mode
:when (doom-point-in-comment-p)

(param "@param " (p "parameter") " " r)
(return "@return " (r "description"))
