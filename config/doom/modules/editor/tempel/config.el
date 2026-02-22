;;; editor/tempel/config.el -*- lexical-binding: t; -*-

(use-package! tempel
  :defer t
  :ghook ('(prog-mode-hook text-mode-hook conf-mode-hook) #'+tempel-setup-capf-h)
  :ghook ('doom-first-buffer-hook #'global-tempel-abbrev-mode)
  :config
  (setq! tempel-path (list (expand-file-name "snippets/*.eld" doom-user-dir)))
  (add-to-list 'tempel-user-elements #'+tempel-add-user-elements)

  (setq! tempel-done-on-region t)

  (map! :map tempel-map
        :gie "TAB"       #'tempel-next
        :gie "<tab>"     #'tempel-next
        :gie "<backtab>" #'tempel-previous
        :gie "S-<tab>"   #'tempel-previous
        :gie "S-TAB"     #'tempel-previous)

  (add-to-list '+corfu-inhibit-auto-functions #'+tempel-active-p)

  (add-hook! 'doom-escape-hook :after
    (defun +tempel-done-h ()
      (when (+tempel-active-p)
        (tempel-done)
        t)))

  ;; HACK: Handle my new elements, R and R>. These are like r and r>, except
  ;; that `tempel-done-on-region' is always treated as nil.
  ;; TODO: PR
  (define-advice tempel--element (:around (fn region elt &rest args) +tempel--add-R-a)
    (if-let* ((elt-r-lowercased
               (pcase elt
                 ('R 'r)
                 ('R> 'r>)
                 (`(R . ,rest) `(r . ,rest))
                 (`(R> . ,rest) `(r> . ,rest)))))
        (let ((tempel-done-on-region nil))
          (apply fn region elt-r-lowercased args))
      (apply fn region elt args))))

(use-package! consult-tempel
  :when (modulep! :completion vertico)
  :defer t
  :init (map! [remap tempel-insert] #'consult-tempel))
