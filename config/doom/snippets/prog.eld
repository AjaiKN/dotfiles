prog-mode
:when (save-excursion (goto-char (pos-bol)) (bobp))

(shebang "#!/usr/bin/env "
         (p
          (if (derived-mode-p 'sh-mode)
              (symbol-name sh-shell)
            (let* ((regexes (cl-loop for (regex . mode) in interpreter-mode-alist
                                     if (eq mode major-mode)
                                     collect regex))
                   (regexes (nreverse regexes))
                   (mode-string (symbol-name major-mode))
                   (mode-simplified (and (string-match (rx bos (group (+? anychar)) (? "-ts") (? "-base") (? "-mode") eos)
                                                       mode-string)
                                         (match-string 1 mode-string)))
                   (parts (cl-loop for regex in regexes
                                   append (mapcar #'car (s-match-strings-all (rx (+ (or alphanumeric "-_"))) regex))))
                   (parts (sort parts :key #'length :reverse t))
                   (parts2 (cl-loop for regex in regexes
                                    append (mapcar #'car (s-match-strings-all (rx (+ alphanumeric)) regex))))
                   (parts2 (sort parts2 :key #'length :reverse t))
                   (together `(,@parts ,@parts2 ,@(and mode-simplified (list mode-simplified))))
                   (together (sort together
                                   :key (lambda (x) (- (+ (if (string-match-p x mode-string) 0.5 0)
                                                     (if (executable-find x) 1 0)
                                                     (if (seq-some (lambda (r) (string-match-p r x)) regexes) 1 0)))))))
              (or (car together)
                  ""))))
         n q
         :post (when (derived-mode-p 'sh-mode) (sh-mode)))
(\#! "#!/usr/bin/env "
         (p
          (if (derived-mode-p 'sh-mode)
              (symbol-name sh-shell)
            (let* ((regexes (cl-loop for (regex . mode) in interpreter-mode-alist
                                     if (eq mode major-mode)
                                     collect regex))
                   (regexes (nreverse regexes))
                   (mode-string (symbol-name major-mode))
                   (mode-simplified (and (string-match (rx bos (group (+? anychar)) (? "-ts") (? "-base") (? "-mode") eos)
                                                       mode-string)
                                         (match-string 1 mode-string)))
                   (parts (cl-loop for regex in regexes
                                   append (mapcar #'car (s-match-strings-all (rx (+ (or alphanumeric "-_"))) regex))))
                   (parts (sort parts :key #'length :reverse t))
                   (parts2 (cl-loop for regex in regexes
                                    append (mapcar #'car (s-match-strings-all (rx (+ alphanumeric)) regex))))
                   (parts2 (sort parts2 :key #'length :reverse t))
                   (together `(,@parts ,@parts2 ,@(and mode-simplified (list mode-simplified))))
                   (together (sort together
                                   :key (lambda (x) (- (+ (if (string-match-p x mode-string) 0.5 0)
                                                     (if (executable-find x) 1 0)
                                                     (if (seq-some (lambda (r) (string-match-p r x)) regexes) 1 0)))))))
              (or (car together)
                  ""))))
         n q
         :post (when (derived-mode-p 'sh-mode) (sh-mode)))
