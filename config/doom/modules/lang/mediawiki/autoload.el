;;; lang/mediawiki/autoload.el -*- lexical-binding: t; -*-

(require 'mediawiki)

(defun +mediawiki--read-page ()
  (let ((hist (cdr (assoc-string mediawiki-site mediawiki-page-history)))
        (temp-history-symbol (make-symbol "mediawiki-temp-history")))
    (set temp-history-symbol (or hist (list (mediawiki-site-first-page mediawiki-site))))
    (read-string "Wiki Page: " (+mediawiki-page-at-point) temp-history-symbol)))

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
(defun +mediawiki/open (name)
  "Open a wiki page specified by NAME from the mediawiki engine."
  (interactive
   (list
    (or (and (not current-prefix-arg)
             (+mediawiki-page-at-point))
        (+mediawiki--read-page))))
  (mediawiki-open name))

;;;###autoload
(defun +mediawiki/open-page-at-point ()
  (interactive)
  (if-let* ((page (+mediawiki-page-at-point)))
      (mediawiki-open page)
    (user-error "no page at point")))

;;;###autoload
(defun +mediawiki-revert-buffer-fn (ignore-auto noconfirm)
  (interactive)
  (if (or (derived-mode-p #'mediawiki-file-mode)
          buffer-file-name)
      (revert-buffer--default ignore-auto noconfirm)
    (mediawiki-reload)))

;;;###autoload
(defun +mediawiki/insert-link ()
  "TODO: similar to org-insert-link"
  (interactive))
