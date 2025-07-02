;;; editor/fold/autoload/fold.el -*- lexical-binding: t; -*-

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which hideshow
;; and `outline-mode' give you). This is my effort to combine them.

;;
;;; Helpers

;;;###autoload
(defun +fold--ensure-hideshow-mode ()
  (or (bound-and-true-p hs-minor-mode)
      (ignore-errors
        (unless comment-start (setq-local comment-start (rx unmatchable)))
        (unless comment-end   (setq-local comment-end   (rx unmatchable)))
        (hs-minor-mode +1)
        t)))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +fold--outline-mode-p (&optional check-headlines-exist allow-outline-indent)
  (unless (or (bound-and-true-p outline-minor-mode)
              (derived-mode-p 'outline-mode)
              (not (fboundp 'outline-indent-minor-mode)))
    (outline-indent-minor-mode))
  (and (or allow-outline-indent
           (not (bound-and-true-p outline-indent-minor-mode)))
       (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (or (not check-headlines-exist)
           (save-excursion
             (goto-char (point-max))
             (outline-previous-visible-heading 1)
             (not (bobp))))))

(defun +fold--outline-fold-p (&optional _ allow-outline-indent)
  (and (+fold--outline-mode-p nil allow-outline-indent)
       (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (when (+fold--ensure-hideshow-mode)
    (save-excursion
      (or (ignore-errors (funcall hs-looking-at-block-start-p-func))
          ;;DIFFERENCE FROM ORIGINAL
          (save-excursion
            (when-let* ((line-num-orig (line-number-at-pos))
                        (block-beginning (ignore-errors (funcall hs-find-block-beginning-func)))
                        (line-num-of-block-beginning (line-number-at-pos block-beginning))
                        (line-num-of-block-end (when hs-minor-mode
                                                 (hs-minor-mode 1)
                                                 (funcall hs-forward-sexp-func 1) ;; go to end of block
                                                 (line-number-at-pos))))
              (and (equal line-num-orig line-num-of-block-beginning)
                   ;; if they're equal, then this block is only one line
                   (not (equal line-num-of-block-beginning line-num-of-block-end)))))
          (unless (eolp)
            (end-of-line)
            (+fold--hideshow-fold-p))))))

(defun +fold--treesit-fold-p ()
  (ignore-errors
    (and (require 'treesit nil t)
         (treesit-available-p)
         (require 'treesit-fold nil t)
         (treesit-fold-usable-mode-p)
         (treesit-fold-ready-p)
         (treesit-fold--foldable-node-at-pos (point)))))

(defun +fold--ts-fold-p ()
  (ignore-errors
    (and (bound-and-true-p tree-sitter-mode)
         (require 'ts-fold nil t)
         (ts-fold-usable-mode-p)
         (ts-fold--foldable-node-at-pos (point)))))

(defun +fold--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (hs-already-hidden-p)
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    points))

(defmacro +fold-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))

(defun +fold--union ()
  "Get the combined region covered by all folds at point."
  ;; We are supporting four folding systems that weren't really designed to work
  ;; together. No doubt users will find novel, unanticipated ways to nest
  ;; different types of folds (especially easy to do with `outline-minor-mode').
  ;; So, we need code that can deal with any arbitrary overlap.
  (cl-reduce
   (lambda (&optional acc cur)
     (when (and acc cur)
       (cons (min (car acc) (car cur))
             (max (cdr acc) (cdr cur)))))
   (nconc
    (when (+fold--vimish-fold-p)
      (mapcar (lambda (ov)
                (cons (overlay-start ov) (overlay-end ov)))
              (seq-filter #'vimish-fold--vimish-overlay-p
                          (or (overlays-at (point)) '()))))
    (when (+fold--outline-fold-p nil t)
      (save-excursion
        (let ((beg (progn (outline-back-to-heading) (point)))
              (end (progn (outline-end-of-subtree) (point))))
          (list (cons beg end)))))
    (when-let ((start (+fold--hideshow-fold-p)))
      ;; `start' could be start of the block, or 't' if that wasn't found.
      ;; In either case, we know the fold is on the same line.
      (let* ((start (or (and (numberp start) start)
                        (line-beginning-position)))
             (end (line-end-position))
             (ov (hs-overlay-at start)))
        (while (and (not ov) (< start end))
          (setq start (next-overlay-change start)
                ov (hs-overlay-at start)))
        (when ov
          (list (cons (overlay-start ov) (overlay-end ov))))))
    (when (+fold--treesit-fold-p)
      (when-let* ((node (treesit-fold--foldable-node-at-pos))
                  (beg (treesit-fold--node-start-position node))
                  (end (treesit-fold--node-end-position node)))
        (list (cons beg end))))
    (when (+fold--ts-fold-p)
      (when-let* ((node (ts-fold--foldable-node-at-pos))
                  (beg (tsc-node-start-position node))
                  (end (tsc-node-end-position node)))
        (list (cons beg end)))))))

(defun +fold--open-rec-between (beg end)
  "Recursively open all folds betwen BEG and END."
  (when (featurep 'vimish-fold)
    ;; from `vimish-fold-unfold-all'
    (mapc #'vimish-fold--unfold
          (vimish-fold--folds-in beg end)))
  (and (+fold--outline-fold-p nil t)
       (outline-show-subtree))
  (hs-life-goes-on
   ;; from `hs-show-all'
   (defvar hs-allow-nesting)
   (let ((hs-allow-nesting nil))
     (hs-discard-overlays beg end))
   (run-hooks 'hs-show-hook))
  (when (+fold--treesit-fold-p)
    (treesit-fold--ensure-ts
      ;; from `ts-fold-open-all'
      (thread-last (overlays-in beg end)
                   (seq-filter
                    (lambda (ov)
                      (eq (overlay-get ov 'invisible) 'treesit-fold)))
                   (mapc #'delete-overlay))))
  (when (+fold--ts-fold-p)
    (ts-fold--ensure-ts
      ;; from `ts-fold-open-all'
      (thread-last (overlays-in beg end)
                   (seq-filter
                    (lambda (ov)
                      (eq (overlay-get ov 'invisible) 'ts-fold)))
                   (mapc #'delete-overlay)))))

(defun +fold--outline-cycle-state ()
  (pcase (outline--cycle-state)
    ('hide-all (if (outline-has-subheading-p)
                   (progn (outline-show-entry)
                          (outline-show-children)
                          (message "Only headings"))
                 (outline-show-entry)
                 (message "Show all")))
    (_ (outline-hide-subtree)
       (message "Hide all"))))

;;
;;; customize buffers

(defun +fold--custom-switch (type)
  "Fold in a Custom (M-x customize) buffer.

TYPE can be either `toggle', t, or nil."
  (let ((point-before (point))
        (was-bol-already (bolp))
        was-bol
        was-open
        toggled)
    (save-excursion
      (beginning-of-line)
      (if (eq (widget-type (widget-at)) 'custom-visibility)
          (progn (setq was-open (widget-value (widget-at)))
                 (setq was-bol t)
                 (setq toggled (or (eq type 'toggle) (not (eq type was-open))))
                 (when toggled
                   (widget-apply-action (widget-at))))
        (end-of-line)
        (backward-char)
        (if (eq (widget-type (widget-at)) 'custom-visibility)
            (progn (setq was-open (widget-value (widget-at)))
                   (setq toggled (or (eq type 'toggle) (not (eq type was-open))))
                   (when toggled
                     (widget-apply-action (widget-at)))))))
    (when toggled
      ;; not sure why regular save-excursion doesn't work
      (goto-char (+ (if (and was-bol (not was-bol-already)) (if was-open 6 -6) 0) point-before)))))

(defun +fold--custom-toggle ()
  (+fold--custom-switch 'toggle))

(defun +fold--custom-open ()
  (+fold--custom-switch t))

(defun +fold--custom-close ()
  (+fold--custom-switch nil))

(defun +fold--custom-open-all ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (+fold--custom-open)
      (forward-line)))
  (recenter))

(defun +fold--custom-close-all ()
  (goto-char (point-max))
  (while (not (bobp))
    (+fold--custom-close)
    (forward-line -1))
  (recenter))

;;
;;; Commands

;;;###autoload
(defun +fold/toggle ()
  "Toggle the fold at point.

Targets `vimish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (cond ((derived-mode-p 'Custom-mode) (+fold--custom-toggle))
        ((+fold--vimish-fold-p) (save-excursion (vimish-fold-toggle)))
        ((+fold--outline-fold-p)
         (save-excursion (+fold--outline-cycle-state)))
        ((+fold--hideshow-fold-p)
         (save-excursion (+fold-from-eol (hs-toggle-hiding))))
        ((+fold--treesit-fold-p) (save-excursion (treesit-fold-toggle)))
        ((+fold--ts-fold-p) (save-excursion (ts-fold-toggle)))
        ((+fold--outline-fold-p nil t)
         (save-excursion (+fold--outline-cycle-state)))))

;;;###autoload
(defun +fold/open-rec ()
  "Recursively open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (cl-destructuring-bind (beg . end) (+fold--union)
    (+fold--open-rec-between beg end)))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (cond ((derived-mode-p 'Custom-mode) (+fold--custom-open))
        ((+fold--vimish-fold-p) (save-excursion (vimish-fold-unfold)))
        ((+fold--outline-fold-p)
         (save-excursion
           (outline-show-branches)
           (outline-show-entry)))
        ((+fold--hideshow-fold-p) (save-excursion (+fold-from-eol (hs-show-block))))
        ((+fold--treesit-fold-p) (save-excursion (treesit-fold-open)))
        ((+fold--ts-fold-p) (save-excursion (ts-fold-open)))
        ((+fold--outline-fold-p nil t)
         (save-excursion
           (outline-show-branches)
           (outline-show-entry)))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (cond ((derived-mode-p 'Custom-mode) (+fold--custom-close))
        ((+fold--vimish-fold-p) (save-excursion (vimish-fold-refold)))
        ((+fold--outline-fold-p) (save-excursion (outline-hide-subtree)))
        ((+fold--hideshow-fold-p) (save-excursion (+fold-from-eol (hs-hide-block))))
        ((+fold--treesit-fold-p) (save-excursion (treesit-fold-close)))
        ((+fold--ts-fold-p) (save-excursion (ts-fold-close)))
        ((+fold--outline-fold-p nil t) (save-excursion (outline-hide-subtree)))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (cond ((derived-mode-p 'Custom-mode)
         (+fold--custom-open-all))
        ((+fold--treesit-fold-p) (treesit-fold-open-all))
        ((+fold--ts-fold-p) (ts-fold-open-all))
        ((and (featurep 'vimish-fold) (+fold--vimish-fold-p))
         (vimish-fold-unfold-all))
        ((save-excursion
           (+fold--ensure-hideshow-mode)
           (if (integerp level)
               (progn
                 (outline-hide-sublevels (max 1 level))
                 (hs-life-goes-on
                  (hs-hide-level-recursive level (point-min) (point-max))))
             (hs-show-all)
             (when (fboundp 'outline-show-all)
               (outline-show-all)))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (cond
     ((derived-mode-p 'Custom-mode)
      (+fold--custom-close-all))
     ((+fold--treesit-fold-p) (treesit-fold-close-all))
     ((+fold--ts-fold-p) (ts-fold-close-all))
     (t
      (when (featurep 'vimish-fold)
        (vimish-fold-refold-all))
      (+fold--ensure-hideshow-mode)
      (if (integerp level)
          (progn
            (outline--show-headings-up-to-level level)
            (hs-life-goes-on
             (hs-hide-level-recursive level (point-min) (point-max))))
        (hs-life-goes-on
         (hs-hide-all))
        (when (fboundp 'outline-hide-sublevels)
          (if outline-indent-minor-mode
              (outline--show-headings-up-to-level 1)
            (outline-show-only-headings))))))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next vimish fold, folded outline heading or folded
region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when (bound-and-true-p hs-block-start-regexp)
                        (car (+fold--invisible-points count))))
                    (lambda ()
                      (when (featurep 'vimish-fold)
                        (if (> count 0)
                            (dotimes (_ count) (vimish-fold-next-fold))
                          (dotimes (_ count)
                            (vimish-fold-previous-fold (- count)))))
                      (if (/= (point) orig-pt) (point)))
                    (lambda ()
                      (when (+fold--outline-mode-p)
                        (cl-destructuring-bind
                            (count fn bound-fn)
                            (if (> count 0)
                                (list count
                                      #'outline-next-visible-heading #'eobp)
                              (list (- count)
                                    #'outline-previous-visible-heading #'bobp))
                          (dotimes (_ count)
                            (funcall fn 1)
                            (outline-end-of-heading)
                            (while (and (not (funcall bound-fn))
                                        (not (outline-invisible-p)))
                              (funcall fn 1)
                              (outline-end-of-heading))))
                        (point)))
                    (lambda ()
                      ;; treesit-fold does not define movement functions so
                      ;; we need to do it ourselves
                      (when (+fold--treesit-fold-p)
                        (let* ((arg-list (if (> count 0) ;; depending on direction we need to change the ranges
                                             (list (point) (point-max))
                                           (list (point-min) (point))))
                               (comp-fun (if (> count 0) ;; also depending on direction we need to change how we sort the list
                                             #'<
                                           #'>))
                               (ovs (cl-remove-if-not
                                     (lambda (ov)
                                       (eq (overlay-get ov 'creator) 'treesit-fold))
                                     ;; `overlays-in' does not provide a list
                                     ;; that is sorted (in the way we need it
                                     ;; atleast) so we need to sort it based on
                                     ;; direction
                                     (cl-sort (apply #'overlays-in arg-list) comp-fun :key #'overlay-start))))
                          (if (and ovs (<= (abs count) (length ovs)))
                              (goto-char (overlay-start (nth (- (abs count) 1) ovs)))))))
                    (lambda ()
                      ;; ts-fold does not define movement functions so
                      ;; we need to do it ourselves
                      (when (+fold--ts-fold-p)
                        (let* ((arg-list (if (> count 0) ;; depending on direction we need to change the ranges
                                             (list (point) (point-max))
                                           (list (point-min) (point))))
                               (comp-fun (if (> count 0) ;; also depending on direction we need to change how we sort the list
                                             #'<
                                           #'>))
                               (ovs (cl-remove-if-not
                                     (lambda (ov)
                                       (eq (overlay-get ov 'creator) 'ts-fold))
                                     ;; `overlays-in' does not provide a list
                                     ;; that is sorted (in the way we need it
                                     ;; atleast) so we need to sort it based on
                                     ;; direction
                                     (cl-sort (apply #'overlays-in arg-list) comp-fun :key #'overlay-start))))
                          (if (and ovs (<= (abs count) (length ovs)))
                              (goto-char (overlay-start (nth (- (abs count) 1) ovs))))))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+fold/next (- count)))

;;;###autoload
(defalias '+fold/create      (if (modulep! :editor evil) #'evil-vimish-fold/create      #'vimish-fold))
;;;###autoload
(defalias '+fold/create-line (if (modulep! :editor evil) #'evil-vimish-fold/create-line #'vimish-fold))
;;;###autoload
(defalias '+fold/delete      #'vimish-fold-delete)
;;;###autoload
(defalias '+fold/delete-all  #'vimish-fold-delete-all)

;;
;;; Global toggling
;; TODO: make this work better using after-change-functions hook or checking that we didn't change anything
;; from https://github.com/alphapapa/outshine/blob/bf1eed10dd7a89b63d0fc014944033db397c1e23/outshine.el#L1724-L1772

;;;###autoload
(defun +fold/outline-cycle-all ()
  (interactive)
  (cond
   ((eq last-command '+fold/overview) (+fold/table-of-contents))
   ((eq last-command '+fold/table-of-contents) (+fold/unfold-all-headings))
   (t (+fold/overview))))

;;;###autoload
(defun +fold/outline-cycle-all-simple ()
  (interactive)
  (if (eq last-command '+fold/table-of-contents)
      (+fold/unfold-all-headings)
    (+fold/table-of-contents)))

;;;###autoload
(defun +fold/overview ()
  "OVERVIEW: Show only top-level headlines.
With a numeric prefix ARG, show all headlines up to that level."
  (interactive)
  (if (+fold--outline-mode-p t)
      (save-excursion
        (let ((toplevel
               (cond
                (current-prefix-arg
                 (prefix-numeric-value current-prefix-arg))
                ((save-excursion
                   (beginning-of-line)
                   (looking-at outline-regexp))
                 (max 1 (save-excursion (beginning-of-line) (funcall outline-level))))
                (t 1))))
          (outline-hide-sublevels toplevel)))
    (+fold/close-all))
  (setq this-command '+fold/overview))

;;;###autoload
(defun +fold/table-of-contents ()
  "CONTENTS: Show all headlines of all levels, but no body text."
  (interactive)
  (when (not (eq last-command '+fold/overview))
    (+fold/overview))
  (if (+fold--outline-mode-p t)
      (save-excursion
        (goto-char (point-max))
        (while (not (bobp))
          (condition-case nil
              (progn
                (outline-previous-visible-heading 1)
                (outline-show-branches))
            (error (goto-char (point-min))))))
    (+fold/close-all))
  (recenter -1)
  (setq this-command '+fold/table-of-contents))

;;;###autoload
(defun +fold/unfold-all-headings ()
  (interactive)
  (when (+fold--outline-mode-p nil t)
    (outline-show-all))
  (+fold/open-all)
  (setq this-command '+fold/unfold-all-headings))

;;
;;; Search headings

;; TODO
;;;###autoload
(defun +fold/all-headings ()
  (interactive))

;;
;;;;;; Drag stuff

;;;###autoload
(defun +fold/drag-stuff-up (&optional whole-indented-block)
  (interactive "P")
  (cond
   ((+fold--outline-fold-p nil whole-indented-block)
    (call-interactively #'outline-move-subtree-up))
   (t (call-interactively #'drag-stuff-up))))

;;;###autoload
(defun +fold/drag-stuff-down (&optional whole-indented-block)
  (interactive "P")
  (cond
   ((+fold--outline-fold-p nil whole-indented-block)
    (call-interactively #'outline-move-subtree-down))
   (t (call-interactively #'drag-stuff-down))))
