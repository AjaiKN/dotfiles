;;; mediawiki-diff.el -*- lexical-binding: t; -*-

(defun mediawiki-diff (sitename id1 id2)
  (let* ((oldid (min id1 id2))
         (newid (max id1 id2))
         (res (mediawiki-api-call
               sitename "query"
               (list
                (cons "prop" (mediawiki-api-param (list "info" "revisions")))
                (cons "revids" (mediawiki-api-param (list (format "%d" newid) (format "%d" oldid))))
                (cons "rvprop" (mediawiki-api-param (list "ids" "timestamp" "user" "flags" "comment" "tags" "content")))
                (cons "rvslots" "main"))))
         (res (cddr res))
         (page (cl-loop for page in (cddr (assq 'pages res))
                        if (eq (car page) 'page)
                        return page))
         (title (mediawiki-page-get-title page))
         (oldstr (mediawiki-page-get-revision page 0 'content))
         (newstr (mediawiki-page-get-revision page 1 'content))
         (oldbuf (get-buffer-create (format " %s revision: %s@%d" sitename title oldid)))
         (newbuf (get-buffer-create (format " %s revision: %s@%d" sitename title newid))))
    (let ((inhibit-read-only t))
      (with-current-buffer oldbuf
        (widen)
        (setq buffer-read-only t)
        (delete-region (point-min) (point-max))
        (insert oldstr)
        (mediawiki-mode))
      (with-current-buffer newbuf
        (widen)
        (setq buffer-read-only t)
        (delete-region (point-min) (point-max))
        (insert newstr)
        (mediawiki-mode)))
    (ediff-buffers oldbuf newbuf)))

;; (mediawiki-diff "Wikipedia" 1346888097 1346175866)
;; (mediawiki-diff "Wikipedia" 1347143285 1316383300)
(mediawiki-diff "Wikipedia" 1347143524 1346888097)
