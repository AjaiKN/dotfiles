(define-module (dotfiles-files)
  #:declarative? #t
  #:export (get-files))

(use-modules (srfi srfi-1)
             (ice-9 ftw)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 string-fun))

(define (path-change-prefix path old-prefix new-prefix)
  (define old-prefix-slash (string-append old-prefix "/"))
  (cond
   [(equal? path old-prefix) new-prefix]
   [(string-prefix? old-prefix-slash path)
    (in-vicinity new-prefix
                 (substring path (string-length old-prefix-slash)))]
   [else
    (throw 'string-doesnt-start-with-prefix (format #f "path ~a doesn't start with old-prefix ~a" path old-prefix))]))

(define* (get-files dir #:optional [location-in-home ""] #:key [transform identity])
  (define (ignore? path)
    (let ((name (basename path)))
      (or (member name '(".profile" ".git" ".gitignore" ".unfold"))
          (any (lambda (rx) (string-match rx name))
               '("~$" "\\.swp$" )))))
  (define (enter? path stat result)
    (and (not (ignore? path))
         (file-exists? (in-vicinity path ".unfold"))))
  (define (leaf path stat result)
    (if (ignore? path)
        result
        (cons
         (list (path-change-prefix path dir location-in-home)
               ;; (local-file path)
               (transform (canonicalize-path path)))
         result)))
  (define (down path stat result)
    result)
  (define (up path stat result)
    result)
  (define (skip path stat result)
    (leaf path stat result))
  (define (error path stat errno result)
    result)
  (file-system-fold enter? leaf down up skip error '() dir
                    ;; follow symlinks
                    stat))

;; (get-files "/home/ajainelson/prog/dotfiles/config" ".config")
;; (get-files "/home/ajainelson/prog/dotfiles/dot-home")
