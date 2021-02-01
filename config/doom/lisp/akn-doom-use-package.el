;;; lisp/akn-doom-use-package.el -*- lexical-binding: t; -*-
;;; Code:

(require 'doom)
(require 'doom-lib)
(doom-require 'doom-lib 'packages)
(load (file-name-concat doom-emacs-dir "modules/config/use-package/init") nil 'nomessage)

(provide 'akn-doom-use-package)
;;; akn-doom-use-package.el ends here
