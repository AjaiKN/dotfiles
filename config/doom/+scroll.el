;;; +scroll.el -*- lexical-binding: t; -*-

;; Scrolling conservatively means it won't recenter as much, which speeds up smooth scrolling a lot.
;; But, for example, when I'm incrementally searching, I want it to recenter, so I temporarily change it back.
(defun akn/do-scroll-conservatively (&rest _)
  (setq scroll-conservatively 101
        scroll-margin 0))
(akn/do-scroll-conservatively)
(defun akn/scroll-progressively (&rest _)
  (unless (bound-and-true-p ultra-scroll--hide-cursor-undo-hook)
    (setq scroll-conservatively 10
          scroll-margin 10)))
(defun akn/do-scroll-conservatively-unless-minibuffer (&rest _)
  (unless (when-let* ((w (minibuffer-window))) (minibuffer-window-active-p w))
    (akn/do-scroll-conservatively)))
(defun akn/scroll-conservatively (&rest _)
  (run-with-idle-timer 0.1 nil
                       #'akn/do-scroll-conservatively-unless-minibuffer))

(advice-add #'smerge-vc-next-conflict :after #'akn/recenter-top)

(add-hook! '(minibuffer-setup-hook isearch-mode-hook) #'akn/scroll-progressively)
(add-hook! '(minibuffer-exit-hook isearch-mode-end-hook) #'akn/scroll-conservatively)
(dolist (fn '(evil-ex-search evil-search
              (evil-ex-start-search . evil-ex-search-abort)
              isearch-printing-char
              (nil . evil-ex-search-exit)
              (evil-ex-search-forward . nil) (evil-search-forward . nil)
              (evil-ex-search-backward . nil) (evil-search-backward . nil)
              semext-forward-part semext-backward-part semext-query-replace semext-search-forward semext-search-backward
              (nil . abort-recursive-edit)
              flycheck-next-error
              flycheck-previous-error
              flycheck-error-list-next-error
              flycheck-error-list-previous-error
              better-jumper-jump-forward
              better-jumper-jump-backward
              better-jumper-jump-newest
              akn/hard-reload-buffer
              akn/reload-buffer
              +fold/overview
              +fold/table-of-contents
              +fold/unfold-all-headings
              +fold/drag-stuff-down
              +fold/drag-stuff-up
              +fold/outline-cycle-all
              +fold/outline-cycle-all-simple
              +fold/next
              +fold/previous
              +fold/toggle
              +fold/open
              +fold/open-all
              +fold/close-all
              +fold/all-headings
              org-cycle
              org-shifttab
              find-file
              +lookup/definition
              visible-mode
              diff-hl-next-hunk
              smerge-next
              smerge-vc-next-conflict
              smerge-prev
              evil-forward-section-begin evil-forward-section-end
              evil-backward-section-begin evil-backward-section-end
              next-error previous-error
              +evil/next-beginning-of-method +evil/next-end-of-method
              ;; goto-line goto-char
              goto-last-change goto-last-change-reverse
              evil-goto-line evil-goto-char evil-goto-error evil-goto-first-line
              evil-goto-last-change evil-goto-last-change-reverse
              evil-goto-mark evil-goto-mark-line))
  (when (not (listp fn))
    (setq fn (cons fn fn)))
  (when (car fn)
    (advice-add (car fn) :before #'akn/scroll-progressively))
  (when (cdr fn)
    (advice-add (cdr fn) :after #'akn/scroll-conservatively)))

(defvar-local +smooth-scroll--end-point-position nil)

(defun +smooth-scroll--pixel-offset ()
  (or (cl-third (pos-visible-in-window-p (window-start) nil t))
      0))
(defun +smooth-scroll--pixel-offset-bottom ()
  (or (cl-fifth (pos-visible-in-window-p (window-start) nil t))
      0))

(when (modulep! :ui smooth-scroll +interpolate)
  (setq! good-scroll-step 1
         good-scroll-duration 0.15)

  ;; TODO: PR
  (after! good-scroll
    (defadvice! good-scroll--scroll-up (fn &optional arg)
      :around #'scroll-up
      (if good-scroll-mode
          (progn
            (setq +smooth-scroll--end-point-position
                  (save-excursion
                    (save-window-excursion
                      (funcall fn arg)
                      (point))))
            (good-scroll-move (- (good-scroll--convert-line-to-step arg)
                                 (+smooth-scroll--pixel-offset))))
        (funcall fn arg)))
    (defadvice! good-scroll--scroll-down (fn &optional arg)
      :around #'scroll-down
      (if good-scroll-mode
          (progn
            (setq +smooth-scroll--end-point-position
                  (save-excursion
                    (save-window-excursion
                      (funcall fn arg)
                      (point))))
            (good-scroll-move (- (+ (good-scroll--convert-line-to-step arg)
                                    (+smooth-scroll--pixel-offset)))))
        (funcall fn arg)))))

;; TODO: add back
;; (when (modulep! :ui smooth-scroll +interpolate)
;;   (defadvice! +smooth-scroll--good-scroll-start-a (&rest _)
;;     :before #'good-scroll-move
;;     ;; (message "start")
;;     (ultra-scroll--maybe-relax-gc)
;;     (defvar ultra-scroll--hide-cursor-start)
;;     (akn/scroll-conservatively)

;;     ;; (let (ultra-scroll--hide-cursor-start)
;;     ;;   (ultra-scroll--hide-cursor (selected-window)))

;;     (push (if (local-variable-p 'cursor-type)
;;               (let ((orig cursor-type))
;;                 (lambda (_v) (setq-local cursor-type orig)))
;;             (lambda (_v) (kill-local-variable 'cursor-type)))
;;           ultra-scroll--hide-cursor-undo-hook)
;;     (setq-local cursor-type nil)
;;     (run-hook-wrapped
;;      'ultra-scroll-hide-functions
;;      (lambda (fun)
;;        (when (or (not (symbolp fun))
;;                  (not (string-suffix-p "-mode" (symbol-name fun)))
;;                  (and (boundp fun) (symbol-value fun)))
;;          (push fun ultra-scroll--hide-cursor-undo-hook)
;;          (funcall fun -1))
;;        nil))

;;     (when (timerp ultra-scroll--gc-timer)
;;       (cancel-timer ultra-scroll--gc-timer)
;;       (setq ultra-scroll--gc-timer nil))
;;     (when (timerp ultra-scroll--hide-cursor-timer)
;;       (cancel-timer ultra-scroll--hide-cursor-timer)
;;       (setq ultra-scroll--hide-cursor-timer nil)))

;;   (defadvice! +smooth-scroll--good-scroll-end-a (fn &rest args)
;;     :around #'good-scroll--render
;;     (let ((before (and (window-valid-p good-scroll--window)
;;                        (not (zerop good-scroll-destination))))
;;           after)
;;       (prog1
;;           (apply fn args)
;;         (setq after (and (window-valid-p good-scroll--window)
;;                          (not (zerop good-scroll-destination))))
;;         (cond
;;          ((and before (not after))
;;           ;; (message "done")
;;           (when +smooth-scroll--end-point-position
;;             (goto-char +smooth-scroll--end-point-position)
;;             (setq +smooth-scroll--end-point-position nil))
;;           (ultra-scroll--restore-gc)
;;           (akn/scroll-conservatively)
;;           (ultra-scroll--hide-cursor-undo (window-buffer good-scroll--window)))))))

;;   (defadvice! +smooth-scroll--good-scroll-scroll-a (target &rest _)
;;     :override #'good-scroll--go-to
;;     (ultra-scroll--scroll (- target) (selected-window))
;;     target))
