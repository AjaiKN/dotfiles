;; guix package --install-from-file=locales.scm
(use-modules (gnu packages base))

(make-glibc-utf8-locales
  glibc
  #:locales (list "en_US")
  #:name "glibc-my-utf8-locales")
