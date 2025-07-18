;;; app/xkcd/autoload.el -*- lexical-binding: t; -*-

;; https://tecosaur.github.io/emacs-config/config.html#xkcd

(require 'xkcd)
(require 'emacsql-sqlite)

;;;###autoload
(defun +xkcd-select ()
  "Prompt the user for an xkcd using `completing-read' and
`+xkcd-select-format'. Return the xkcd number or nil"
  (let* (prompt-lines
         (_-dummy (maphash (lambda (_key xkcd-info))
                           (push (+xkcd-select-format xkcd-info) prompt-lines)
                         +xkcd-stored-info))
         (num (completing-read (format "xkcd (%s): " xkcd-latest) prompt-lines)))
    (if (equal "" num) xkcd-latest
      (string-to-number (replace-regexp-in-string "\\([0-9]+\\).*" "\\1" num)))))

;;;###autoload
(defun +xkcd-select-format (xkcd-info)
  "Creates each completing-read line from an xkcd info plist. Must
start with the xkcd number"
  (format "%-4s  %-30s %s"
          (propertize (number-to-string (plist-get xkcd-info :num))
                      'face 'counsel-key-binding)
          (plist-get xkcd-info :title)
          (propertize (plist-get xkcd-info :alt)
                      'face '(variable-pitch font-lock-comment-face))))

;;;###autoload
(defun +xkcd-fetch-info (&optional num)
  "Fetch the parsed json info for comic NUM. Fetches latest when omitted or 0"
  (when (or (not num) (= num 0))
    (+xkcd-check-latest)
    (setq num xkcd-latest))
  (let ((res (or (gethash num +xkcd-stored-info)
                 (puthash num (+xkcd-db-read num) +xkcd-stored-info))))
    (unless res
      (+xkcd-db-write
       (let* ((url (format "https://xkcd.com/%d/info.0.json" num))
              (json-assoc
               (if (gethash num +xkcd-stored-info)
                   (gethash num +xkcd-stored-info)
                 (json-read-from-string (xkcd-get-json url num)))))
         json-assoc))
      (setq res (+xkcd-db-read num)))
    res))

;; since we've done this, we may as well go one little step further
;;;###autoload
(defun +xkcd-find-and-copy ()
  "Prompt for an xkcd using `+xkcd-select' and copy url to clipboard"
  (interactive)
  (+xkcd-copy (+xkcd-select)))

;;;###autoload
(defun +xkcd-copy (&optional num)
  "Copy a url to xkcd NUM to the clipboard"
  (interactive "i")
  (let ((num (or num xkcd-cur)))
    (gui-select-text (format "https://xkcd.com/%d" num))
    (message "xkcd.com/%d copied to clipboard" num)))

;;;###autoload
(defun +xkcd-find-and-view ()
  "Prompt for an xkcd using `+xkcd-select' and view it"
  (interactive)
  (xkcd-get (+xkcd-select))
  (switch-to-buffer "*xkcd*"))

(defvar +xkcd-latest-max-age (* 60 60) ; 1 hour
  "Time after which xkcd-latest should be refreshed, in seconds")

;;;###autoload
(defun +xkcd-check-latest ()
  "Use value in `xkcd-cache-latest' as long as it isn't older thabn
`+xkcd-latest-max-age'"
  (unless (and (file-exists-p xkcd-cache-latest)
               (< (- (time-to-seconds (current-time))
                     (time-to-seconds (file-attribute-modification-time (file-attributes xkcd-cache-latest))))
                  +xkcd-latest-max-age))
    (let* ((out (xkcd-get-json "http://xkcd.com/info.0.json" 0))
           (json-assoc (json-read-from-string out))
           (latest (cdr (assoc 'num json-assoc))))
      (when (/= xkcd-latest latest)
        (+xkcd-db-write json-assoc)
        (with-current-buffer (find-file xkcd-cache-latest)
          (setq xkcd-latest latest)
          (erase-buffer)
          (insert (number-to-string latest))
          (save-buffer)
          (kill-buffer (current-buffer)))))
    (shell-command (format "touch %s" xkcd-cache-latest))))

(defvar +xkcd-stored-info (make-hash-table :test 'eql)
  "Basic info on downloaded xkcds, in the form of a hashtable")

(defconst +xkcd-db--sqlite-available-p
  (with-demoted-errors "+org-xkcd initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

(defvar +xkcd-db--connection (make-hash-table :test #'equal)
  "Database connection to +org-xkcd database.")

;;;###autoload
(defun +xkcd-db--get ()
  "Return the sqlite db file."
  (expand-file-name "xkcd.db" xkcd-cache-dir))

;;;###autoload
(defun +xkcd-db--get-connection ()
  "Return the database connection, if any."
  (gethash (file-truename xkcd-cache-dir)
           +xkcd-db--connection))

(defconst +xkcd-db--table-schema
  '((xkcds
     [(num integer :unique :primary-key)
      (year        :not-null)
      (month       :not-null)
      (link        :not-null)
      (news        :not-null)
      (safe_title  :not-null)
      (title       :not-null)
      (transcript  :not-null)
      (alt         :not-null)
      (img         :not-null)])))

;;;###autoload
(defun +xkcd-db--init (db)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) +xkcd-db--table-schema)
      (emacsql db [:create-table $i1 $S2] table schema))))

;;;###autoload
(defun +xkcd-db ()
  "Entrypoint to the +org-xkcd sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and (+xkcd-db--get-connection)
               (emacsql-live-p (+xkcd-db--get-connection)))
    (let* ((db-file (+xkcd-db--get))
           (init-db (not (file-exists-p db-file))))
      (make-directory (file-name-directory db-file) t)
      (let ((conn (emacsql-sqlite db-file)))
                                        ; (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (file-truename xkcd-cache-dir)
                 conn
                 +xkcd-db--connection)
        (when init-db
          (+xkcd-db--init conn)))))
  (+xkcd-db--get-connection))

;;;###autoload
(defun +xkcd-db-query (sql &rest args)
  "Run SQL query on +org-xkcd database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if  (stringp sql)
      (emacsql (+xkcd-db) (apply #'format sql args))
    (apply #'emacsql (+xkcd-db) sql args)))

;;;###autoload
(defun +xkcd-db-read (num)
  (when-let ((res
              (car (+xkcd-db-query [:select * :from xkcds
                                    :where (= num $s1)]
                                   num
                                   :limit 1))))
    (+xkcd-db-list-to-plist res)))

;;;###autoload
(defun +xkcd-db-read-all ()
  (let ((xkcd-table (make-hash-table :test 'eql :size 4000)))
    (mapc (lambda (xkcd-info-list)
            (puthash (car xkcd-info-list) (+xkcd-db-list-to-plist xkcd-info-list) xkcd-table))
          (+xkcd-db-query [:select * :from xkcds]))
    xkcd-table))

;;;###autoload
(defun +xkcd-db-list-to-plist (xkcd-datalist)
  `(:num ,(nth 0 xkcd-datalist)
    :year ,(nth 1 xkcd-datalist)
    :month ,(nth 2 xkcd-datalist)
    :link ,(nth 3 xkcd-datalist)
    :news ,(nth 4 xkcd-datalist)
    :safe-title ,(nth 5 xkcd-datalist)
    :title ,(nth 6 xkcd-datalist)
    :transcript ,(nth 7 xkcd-datalist)
    :alt ,(nth 8 xkcd-datalist)
    :img ,(nth 9 xkcd-datalist)))

;;;###autoload
(defun +xkcd-db-write (data)
  (+xkcd-db-query [:insert-into xkcds
                   :values $v1]
                  (list (vector
                         (cdr (assoc 'num        data))
                         (cdr (assoc 'year       data))
                         (cdr (assoc 'month      data))
                         (cdr (assoc 'link       data))
                         (cdr (assoc 'news       data))
                         (cdr (assoc 'safe_title data))
                         (cdr (assoc 'title      data))
                         (cdr (assoc 'transcript data))
                         (cdr (assoc 'alt        data))
                         (cdr (assoc 'img        data))))))

;;;###autoload
(defun +xkcd--initialize ()
  (require 'xkcd)
  (+xkcd-fetch-info xkcd-latest)
  (setq +xkcd-stored-info (+xkcd-db-read-all)))
