mediawiki-mode

(h1 & "= "      r      " =" %)
(h2 & "== "     r     " ==" %)
(h3 & "=== "    r    " ===" %)
(h4 & "==== "   r   " ====" %)
(h5 & "===== "  r  " =====" %)
(h6 & "====== " r " ======" %)

;; (code "<code>" r "</code>")
(code "<syntaxhighlight " " lang="p " inline" ">" r "</syntaxhighlight>")
(codeblock & "<syntaxhighlight" " lang="p " line" " linelinks" " copy" ">" n r n "</syntaxhighlight>" %)
(wikitext  & "<syntaxhighlight" " lang=wikitext"               " copy" ">" n r n "</syntaxhighlight>" %)

(latex "{{nowrap|<math>" r "</math>}}")
(latexblock & "<math display=block>" r "</math>" %)

(i "''"      r "''") ;italics
(b "''''"    r "''''") ;bold
(bi "''''''" r "''''''") ;bold italics
(nobold "{{nobold|" r "}}")
(noitalic "{{noitalic|" r "}}")

(tag "<"(p "tag" element)">" r "<"element"/>")

(small "<small>" r "</small>")
(big "<big>" r "</big>")
(sup "<sup>" r "</sup>")
(sub "<sub>" r "</sub>")

(nbsp "&nbsp;")
(nowrap "{{nowrap|" r "}}")

(pad "{{pad|" p "em}}")

(link "[[" (R "target") "|" p "]]")
(linksection "{{section link|" r "}}")
(wp "[[WP:" r "]]")
(help "[[Help:" r "]]")
(wikt "[[wikt:" R "|" p "]] ")

(addcat "[[Category:" (r "Name (not a link)") "]] ")
(linkcat "[[:Category:" R "|" p "]] ")

(redirect "#REDIRECT [[" r "]]")

(book "<ref>{{cite book"
      " |last=" p
      " |first=" p
      " |date=" p
      " |title=" R
      " |url=" p
      " |location=" p
      " |publisher=" p
      " |page=" p
      " |isbn=" p
      " |access-date=" p
      "}}</ref>")
(web "<ref>{{cite web"
     " |last=" p
     " |first=" p
     " |date=" p
     " |title=" p
     " |url=" R
     " |website=" p
     " |location=" p
     " |publisher=" p
     " |url-status=" (p "live")
     " |archive-url=" p
     " |archive-date=" p
     " |access-date=" (p (format-time-string "%Y-%m-%d"))
     "}}</ref>")
(news "{{cite news"
      " |last=" p
      " |first=" p
      " |date=" p
      " |title=" p
      " |url=" R
      " |work=" p
      " |location=" p
      " |publisher=" p
      " |url-status=" (p "live")
      " |archive-url=" p
      " |archive-date=" p
      " |access-date=" (p (format-time-string "%Y-%m-%d"))
      "}}")

(ref "<ref name=\"" p "\" />")
(cn "{{Citation needed|{{subst:DATE}}}}")

(blockquote & "{{blockquote |text="R" |sign="p" }}" %)
