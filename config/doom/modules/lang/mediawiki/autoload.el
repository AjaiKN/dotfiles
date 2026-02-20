;;; lang/mediawiki/autoload.el -*- lexical-binding: t; -*-

(require 'mediawiki)

(defun +mediawiki--read-page (site)
  (let* ((mediawiki-site site)
         (hist (cdr (assoc-string mediawiki-site mediawiki-page-history)))
         (temp-history-symbol (make-symbol "mediawiki-temp-history"))
         (start-page (mediawiki-site-first-page mediawiki-site)))
    (set temp-history-symbol (or hist (list start-page)))
    (read-string "Wiki Page: " (+mediawiki-page-at-point) temp-history-symbol start-page)))

;;;###autoload
(defun +mediawiki-page-at-point ()
  (ignore-errors (mediawiki-page-at-point)))

;;;###autoload
(defun +mediawiki-file-p ()
  (or (derived-mode-p #'mediawiki-file-mode)
      buffer-file-name))

;;;###autoload
(defun +mediawiki-virtual-p ()
  (and (derived-mode-p #'mediawiki-mode)
       (not (+mediawiki-file-p))))

;;;###autoload
(defun +mediawiki/save ()
  (interactive)
  (when (buffer-modified-p)
    (call-interactively #'mediawiki-save)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun +mediawiki/open (site page)
  (interactive
   (let* ((mediawiki-site
           (if current-prefix-arg
               (mediawiki-prompt-for-site)
             (or mediawiki-site mediawiki-site-default))))
     (list
      mediawiki-site
      (or (and (not current-prefix-arg)
               (+mediawiki-page-at-point))
          (+mediawiki--read-page mediawiki-site)))))
  (unless (mediawiki-logged-in-p site)
    (mediawiki-do-login site))
  (if-let* ((page (mediawiki-translate-pagename page))
            (bufname (concat site ": " page))
            (buf (get-buffer bufname))
            ((with-current-buffer buf (derived-mode-p #'mediawiki-mode))))
      (progn
        (mediawiki-pop-to-buffer buf)
        (setq-local mediawiki-site site
            mediawiki-page-title page)
        (unless (buffer-modified-p)
          (revert-buffer t)))
    (mediawiki-edit site page)))

;;;###autoload
(defun +mediawiki/open-page-at-point ()
  (interactive)
  (if-let* ((page (+mediawiki-page-at-point)))
      (+mediawiki/open (or mediawiki-site mediawiki-site-default) page)
    (user-error "no page at point")))

;;;###autoload
(defun +mediawiki-revert-buffer-fn (ignore-auto noconfirm)
  (if (or (derived-mode-p #'mediawiki-file-mode)
          buffer-file-name)
      (revert-buffer--default ignore-auto noconfirm)
    (run-hooks 'before-revert-hook)
    (unless (mediawiki-logged-in-p mediawiki-site)
      (mediawiki-do-login mediawiki-site))
    (setq-local buffer-file-coding-system 'utf-8
                mediawiki-page-title (mediawiki-translate-pagename mediawiki-page-title))
    ;; Get page content and metadata, ensuring metadata is saved in current buffer
    (let* ((page-data (mediawiki-get mediawiki-site mediawiki-page-title))
           (page (car page-data))
           (content (cdr page-data))
           (tmpfile (shut-up (make-temp-file "mediawiki" nil nil (or content ""))))
           (inhibit-read-only t)
           (buffer-file-name tmpfile))
      (clear-visited-file-modtime)
      (mediawiki-save-metadata mediawiki-site page)
      (akn/letf! ((#'set-visited-file-modtime #'ignore))
        (funcall
          (or revert-buffer-insert-file-contents-function
              #'revert-buffer-insert-file-contents--default-function)
          tmpfile nil))
      (clear-visited-file-modtime))
    (set-buffer-modified-p nil)
    (run-hooks 'after-revert-hook)))

;;;###autoload
(defun +mediawiki/insert-link ()
  "TODO: similar to org-insert-link"
  (interactive))
