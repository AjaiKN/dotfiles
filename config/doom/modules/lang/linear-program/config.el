;;; lang/linear-program/config.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'akn-doom-use-package)
  (require 'doom-keybinds))

(use-package! linear-program-mode
  :defer t
  :mode "\\.lp\\'"
  :config
  (add-hook 'linear-program-mode-hook #'rainbow-delimiters-mode)

  (defun +linear-program-run-on-file (fmt)
    (compilation-start (format fmt
                               (shell-quote-argument (or (buffer-file-name)
                                                         (let ((tmp (make-temp-file "tmp" nil ".lp")))
                                                           (without-restriction
                                                             (write-region (point-min) (point-max) tmp)
                                                             tmp)))))))

  (defun +linear-program/glpsol ()
    (interactive nil linear-program-mode)
    (+linear-program-run-on-file "rm /tmp/lp-log.log /tmp/lp-result.sol 2>/dev/null 2>&1; glpsol --lp %s -o /tmp/lp-result.sol >/tmp/lp-log.log && cat /tmp/lp-result.sol || cat /tmp/lp-log.log"))

  (defun +linear-program/gurobi ()
    (interactive nil linear-program-mode)
    (+linear-program-run-on-file "rm /tmp/lp-log.log /tmp/lp-result.sol 2>/dev/null 2>&1; gurobi_cl Resultfile=/tmp/lp-result.sol Logfile=/tmp/lp-log.log Method=0 %s >/dev/null && cat /tmp/lp-result.sol || cat /tmp/lp-log.log"))

  (defun +linear-program/highs ()
    (interactive nil linear-program-mode)
    (+linear-program-run-on-file "rm /tmp/lp-log.log /tmp/lp-result.sol 2>/dev/null 2>&1; highs --solution_file /tmp/lp-result.sol %s >/tmp/lp-log.log && cat /tmp/lp-result.sol || cat /tmp/lp-log.log"))

  (defun +linear-program/fixup ()
    (interactive nil linear-program-mode)
    (save-excursion
      (while (and (not (buffer-narrowed-p))
                  (not (string-match-p (rx bol (not (any space "\r\n"))) (buffer-string)))
                  (string-match-p (rx bol (+ space) (not (any space "\r\n"))) (buffer-string)))
        (indent-rigidly-left (point-min) (point-max)))
      (goto-char (point-min))
      (replace-regexp-in-region (rx (or (any ?\\ ?\" "$&")
                                        (seq (any ".,") (* blank) eol)
                                        (seq (+ blank) eol)
                                        (seq " " (or "thin" "med" "thick" "quad" "wide") word-end)))
                                "")
      (replace-regexp-in-region (rx "≥") ">=")
      (replace-regexp-in-region (rx "≤") "<=")
      (replace-regexp-in-region (rx (group (not (any blank "+" "*" "/" "-" "=" "<" ">")))
                                    (group (or "+" "*" "/" "-" "=" "<=" ">=" "<" ">")))
                                "\\1 \\2")
      (replace-regexp-in-region (rx (group (or "+" "*" "/" "-" "=" "<=" ">=" "<" ">"))
                                    (group (not (any blank "+" "*" "/" "-" "=" "<" ">"))))
                                "\\1 \\2")
      (replace-regexp-in-region (rx " " (group (+ (any digit "."))) (group alpha))
                                " \\1 \\2")
      (goto-char (point-min))
      (while (re-search-forward (rx bol
                                    (group (* blank))
                                    (group (or "maximize" "minimize" "st" "such that" "s.t." "subject to"))
                                    (group (+ blank) (+ anything))
                                    eol)
                                nil
                                t)
        (replace-match (concat (match-string 2) "\n"
                               (match-string 1) (make-string (length (match-string 2)) ?\s) (match-string 3)))
        (goto-char (point-min)))
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (unless (string-match-p (rx "bounds") (buffer-string))
        (goto-char (point-max))
        (insert "bounds\n"))
      (unless (string-match-p (rx "end" (* (any space ?\r ?\n)) eos) (buffer-string))
        (goto-char (point-max))
        (insert "end\n"))
      (goto-char (point-min))
      (replace-regexp-in-region (rx (+ blank) eol)
                                "")))

  (map! :map linear-program-mode-map
        (:prefix "C-c"
         "f" #'+linear-program/fixup
         :desc "solve with gurobi" "g" #'+linear-program/gurobi
         :desc "solve with glpsol" "G" #'+linear-program/glpsol
         :desc "solve with HiGHS"  "h" #'+linear-program/highs)
        (:localleader
         "f" #'+linear-program/fixup
         :desc "solve with gurobi" "g" #'+linear-program/gurobi
         :desc "solve with glpsol" "G" #'+linear-program/glpsol
         :desc "solve with HiGHS"  "h" #'+linear-program/highs)))

(use-package! minizinc-ts-mode
  :defer t
  :config
  (add-hook 'minizinc-ts-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'minizinc-ts-mode-hook #'treesit-inspect-mode)

  (after! dtrt-indent
   (add-to-list 'dtrt-indent-hook-mapping-list '(minizinc-ts-mode default minizinc-ts-indent-offset)))
  (after! editorconfig
    (add-to-list 'editorconfig-indentation-alist '(minizinc-ts-mode minizinc-ts-indent-offset)))
  (after! lsp-mode
    (add-to-list 'lsp--formatting-indent-alist '(minizinc-ts-mode . minizinc-ts-indent-offset)))

  (set-ligatures! 'minizinc-ts-mode
    :and       "/\\"
    :or        "\\/"
    :not       "not"
    :in        "in"
    :some      "exists"
    :for       "forall"
    :union     "union"
    :intersect "intersect"
    :sum       "sum"
    :ceil      "ceil"
    :floor     "floor"
    :true      "true"
    :false     "false"
    :int       "int"
    :float     "float"
    :str       "string"
    :bool      "bool"
    :list      "list"
    :tuple     "tuple"))
