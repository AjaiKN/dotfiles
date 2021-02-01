;;; akn-test.el -*- lexical-binding: t; -*-
;;; Code:

(require 'akn)

(ert-deftest akn/read-file-name ()
  (should (equal (macroexpand-1
                  '(akn/read-file-name "Rename visited file to: " :initial (file-name-nondirectory buffer-file-name)))
                 '(read-file-name "Rename visited file to: " nil nil nil (file-name-nondirectory buffer-file-name) nil))))

(provide 'akn-test)
;;; akn-test.el ends here
