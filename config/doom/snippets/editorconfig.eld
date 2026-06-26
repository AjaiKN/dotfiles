editorconfig-conf-mode

(template "# See https://editorconfig.org" n
          n
          "# top-most EditorConfig file" n
          "root = " (p "true") n
          n
          "[*]" n
          "indent_style = " (p "tab") n
          "# indent_size = " (p "2") n
          "end_of_line = lf" n
          "charset = utf-8" n
          "trim_trailing_whitespace = " (p "true") n
          "insert_final_newline = " (p "true") n
          "# max_line_length = " (p "80") n
          "spelling_language = en-US" n
          n
          "[*.md]" n
          "trim_trailing_whitespace = false" n
          n
          "[Makefile]" n
          (i tabs) n)

(all & "[*]" %)
(tabs & "indent_style = tab" n
      "indent_size = unset" %)
(spaces & "indent_style = space" n
        "indent_size = " (p "2") %)
