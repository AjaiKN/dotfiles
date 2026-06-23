;;; lang/mediawiki/mediawiki-hist.el -*- lexical-binding: t; -*-

(require 'mediawiki-core)
(require 'mediawiki-api)

(defvar-local mediawiki-hist-num-revs 100)

(define-derived-mode mediawiki-hist-mode tabulated-list-mode "MediaWiki Hist"
  "Major mode for listing MediaWiki revisions."
  (setq tabulated-list-format [;("id")
                               ("timestamp" 20 t)
                               ("user"      20 t)
                               ;; ("delta"      8 t :right-align t)
                               ("comment"    0 t)]
        tabulated-list-entries #'mediawiki-hist-entries
        fill-prefix (make-string (+ 20 1 15 1 0) ?\s)
        adaptive-fill-function (lambda () fill-prefix))
  (add-hook 'tabulated-list-revert-hook #'tabulated-list-init-header nil 'local))

(map! :map mediawiki-mode-map
      [remap magit-log-buffer-file] #'mediawiki-hist)
(map! :map mediawiki-hist-mode-map
      :niemg "RET" #'mediawiki-diff)

(defun mediawiki-hist-entries ()
  (let* ((res
          (mediawiki-api-query-revisions
           mediawiki-site mediawiki-page-title
           (list "ids" "timestamp" "user" "flags" "comment" "tags")
           mediawiki-hist-num-revs))
         (page (mediawiki-pagelist-find-page res mediawiki-page-title))
         (revisions (cdr (alist-get 'revisions page))))
    (cl-loop for rev in revisions
             collect
             (let-alist (cadr rev)
               (list .revid
                     (vector (propertize .timestamp 'face 'magit-log-date)
                             (propertize .user '     face 'magit-log-author)
                             ;; (propertize "" '        face 'magit-diffstat-added)
                             (concat (if .minor (propertize "m. " 'face 'font-lock-comment-face) "")
                                     (propertize .comment 'face 'italic)
                                     (if-let* ((tags (cddr (alist-get 'tags rev)))
                                               (tags (cl-loop for tag in tags
                                                              if (eq (car tag) 'tag)
                                                              collect (propertize (caddr tag) 'face 'magit-tag))))
                                         (format "%s(%s)" (if (equal .comment "") "" " ") (string-join tags ", "))
                                       ""))))))))

;;;###autoload
(cl-defun mediawiki-hist (&optional (sitename (or mediawiki-site mediawiki-site-default))
                                    (title mediawiki-page-title)
                                    (num-revs mediawiki-hist-num-revs))
  (interactive)
  (let ((buf (get-buffer-create (format "*hist: %s: %s*" sitename title))))
    (with-current-buffer buf
      (mediawiki-hist-mode)
      (setq-local mediawiki-site sitename
                  mediawiki-page-title title
                  mediawiki-hist-num-revs num-revs)
      (revert-buffer t t t))
    (doom-set-buffer-real buf t)
    (pop-to-buffer buf)))
