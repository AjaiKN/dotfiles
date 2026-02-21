;;; snippets

;; some of these are taken from:
;; https://github.com/Crandel/tempel-collection
;; https://github.com/doomemacs/snippets

;;;; general
fundamental-mode

(tdy (format-time-string "%Y-%m-%d"))
(nw (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))
(ystrd (format-time-string "<%Y-%0m-%0d %a>" (time-subtract nil (* 24 60 60))))
(tmrrow (format-time-string "<%Y-%0m-%0d %a>" (time-add nil (* 24 60 60))))

;;;; specific
editorconfig-conf-mode

(template "root = true

[*]
indent_style = tab
# indent_size = 2
end_of_line = lf
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true
# max_line_length = 80
spelling_language = en-US

[*.md]
trim_trailing_whitespace = false

[Makefile]
indent_style = tab
")

emacs-lisp-mode

(au & ";;;###autoload")

(lexical "-*- lexical-binding: t -*-")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
				" --- " p " " (i lexical) " -*-" n
				";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
				 ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
				 " ends here" n)
(package (i header) r n n (i provide))

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

(link "[[" (r "target") "|" p "]]")
(linksection "{{section link|" r "}}")
(wp "[[WP:" r "]]")
(help "[[Help:" r "]]")
(wikt "[[wikt:" r "|" p "]] ")

(addcat "[[Category:" (r "Name (not a link)") "]] ")
(linkcat "[[:Category:" r "|" p "]] ")

(redirect "#REDIRECT [[" r "]]")

(book "<ref>{{cite book"
			" |last=" p
			" |first=" p
			" |date=" p
			" |title=" r
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
		 " |url=" r
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
			" |url=" r
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

(blockquote & "{{blockquote |text="r" |sign="p" }}" %)

sh-mode

(cd_dirname "cd \"$(dirname \"\$0\")\"")
(safemode & "set -euo pipefail" n
					"# shellcheck disable=SC2154" n
					"trap 's=$?; echo >&2 \"$0: Error on line \"$LINENO\": $BASH_COMMAND\"; exit $s' ERR" n)
(verbose "set -x")
(__bash (if (bobp) "#!/usr/bin/env bash\n\n")
				(i safemode)
				(i verbose) n n)

;;; file-local variables
;; Local Variables:
;; mode: lisp-data
;; eval: (prettify-symbols-mode -1)
;; eval: (auto-composition-mode -1)
;; outline-regexp: "[a-z-]+ *$\\|;;;\\(;*\\) [^ \t\n]"
;; outline-level: (lambda () (if (match-beginning 1) (- (match-end 1) (match-beginning 1)) 7))
;; End:
