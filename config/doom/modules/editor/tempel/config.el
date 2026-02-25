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
      (apply fn region elt args)))

  (defvar +tempel-region-string)
  (defvar +tempel--used-yasnippet-p)
  (define-advice tempel--insert (:around (fn template region &rest args) +tempel--add-region)
    (let ((+tempel-region-string (and-let* ((beg (car region))
                                            (end (cdr region)))
                                      (buffer-substring-no-properties beg end)))
          (+tempel--used-yasnippet-p nil))
      (prog1
          (apply fn template region args)
        ;; If the region wasn't used directly because it was used by yasnippet's
        ;; yas-selected-text, it'll still be laying around after the template.
        ;; In that case, remove it.
        (when-let* ((+tempel--used-yasnippet-p)
                    (ov (tempel--find-overlay 'tempel--range))
                    (template-beg (overlay-start ov))
                    (template-end (overlay-end ov))
                    (region-end (cdr region))
                    (region-end (marker-position region-end))
                    ((> region-end template-end))
                    (region-beg template-end))
          (delete-region region-beg region-end)))))

  ;; I'm not sure why tempel doesn't make (p (expression...)) placeholders
  ;; overwrite the expression result when you start typing, even though for
  ;; (p "string") it does.
  (define-advice tempel--field (:around (fn &optional name init _default) +tempel--always-allow-overwrite)
    (funcall fn name init t)))

(use-package! consult-tempel
  :when (modulep! :completion vertico)
  :defer t
  :init (map! [remap tempel-insert] #'consult-tempel))
