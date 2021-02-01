;;; emacs/vlf/autoload.el -*- lexical-binding: t; -*-

;; Largely taken from:
;; https://tecosaur.github.io/emacs-config/config.html#very-large-files

;;;###autoload
(defun +vlf--files--ask-user-about-large-file-a (size op-type filename offer-raw)
  "Like `files--ask-user-about-large-file', but with support for `vlf'."
  (if (eq vlf-application 'dont-ask)
      (progn (vlf filename) (error ""))
    (let ((prompt (format "File %s is large (%s), really %s?"
                          (file-name-nondirectory filename)
                          (funcall byte-count-to-string-function size) op-type)))
      (if (not offer-raw)
          (if (y-or-n-p prompt) nil 'abort)
        (let ((choice
               (car
                (read-multiple-choice
                 prompt '((?y "yes")
                          (?n "no")
                          (?l "literally")
                          (?v "vlf"))
                 (files--ask-user-about-large-file-help-text
                  op-type (funcall byte-count-to-string-function size))))))
          (cond ((eq choice ?y) nil)
                ((eq choice ?l) 'raw)
                ((eq choice ?v)
                 (vlf filename)
                 (error ""))
                (t 'abort)))))))

(defvar-local +vlf--cumulative-linenum '((0 . 0))
  "An alist keeping track of the cumulative line number.")
;;;###autoload
(defun +vlf-update-linum ()
  "Update the line number offset."
  (let ((linenum-offset (alist-get vlf-start-pos +vlf--cumulative-linenum)))
    (setq display-line-numbers-offset (or linenum-offset 0))
    (when (and linenum-offset (not (assq vlf-end-pos +vlf--cumulative-linenum)))
      (push (cons vlf-end-pos (+ linenum-offset
                                 (count-lines (point-min) (point-max))))
            +vlf--cumulative-linenum))))

;;;###autoload
(defun +vlf-isearch-wrap ()
  (if isearch-forward
      (+vlf--next-chunk-or-start)
    (+vlf--last-chunk-or-end)))
(defun +vlf--next-chunk-or-start ()
  (if (= vlf-file-size vlf-end-pos)
      (vlf-jump-to-chunk 1)
    (vlf-next-batch 1))
  (goto-char (point-min)))
(defun +vlf--last-chunk-or-end ()
  (if (= 0 vlf-start-pos)
      (vlf-end-of-file)
    (vlf-prev-batch 1))
  (goto-char (point-max)))

;;;###autoload
(defun +vlf/beginning-of-file ()
  (interactive)
  (vlf-beginning-of-file)
  (goto-char (point-min)))

;;;###autoload
(defun +vlf/end-of-file ()
  (interactive)
  (vlf-end-of-file)
  (goto-char (point-max)))

;;;###autoload
(defun +vlf-first-chunk-p ()
  (or (not vlf-mode)
      (eq vlf-start-pos 0)))
;;;###autoload
(defun +vlf-last-chunk-p ()
  (or (not vlf-mode)
      (progn
        (vlf-verify-size)
        (eq vlf-end-pos vlf-file-size))))

;;;###autoload
(defun +vlf-bobp ()
  (and (bobp)
       (+vlf-first-chunk-p)))
;;;###autoload
(defun +vlf-eobp (&optional allow-evil)
  (and (if (and allow-evil (fboundp 'evil-eobp)) (evil-eobp) (eobp))
       (+vlf-last-chunk-p)))

;;;###autoload
(defun +vlf--scroll-up-a (orig-fun &rest args)
  "Slide to next batch if at end of buffer in `vlf-mode'."
  (if (and vlf-mode
           (pos-visible-in-window-p (point-max))
           ;; ADDED
           (not (+vlf-last-chunk-p)))
      (progn (vlf-next-batch 1)
             (goto-char (point-min)))
    (apply orig-fun args)))

;;;###autoload
(defun +vlf/enable-follow (&optional interval)
  (interactive
   (list
    (when current-prefix-arg
      (read-number "Interval before VLF recentering (seconds): " 1))))
  (require 'vlf-follow)
  (+vlf/disable-follow)
  (vlf-start-follow (or interval 0.5)))
;;;###autoload
(defun +vlf/disable-follow ()
  (interactive)
  (when (bound-and-true-p vlf-follow-timer) (vlf-stop-follow)))

;;;###autoload
(defun +vlf--recenter-a (vlf-buffer)
  (and vlf-follow-timer
       (eq (current-buffer) vlf-buffer)
       ;; ADDED
       (> vlf-file-size
          ;; check that there's not just one batch
          vlf-batch-size
          ;; check that batch size is big enough to not entirely fit in window
          (* 2 (window-width) (window-height)))
       ;;                                             ADDED: don't recenter if at beginning or end
       (or (and (pos-visible-in-window-p (point-min)) (not (+vlf-first-chunk-p)))
           (and (pos-visible-in-window-p (point-max)) (not (+vlf-last-chunk-p))))))

;;;###autoload
(defun +vlf--evil-goto-line-a (fn count)
  (if vlf-mode
      (evil-ensure-column
        (if (null count)
            (+vlf/end-of-file)
          (vlf-goto-line count)))
    (funcall fn count)))

;;;###autoload
(define-minor-mode +vlf-auto-revert-tail-mode
  ""
  :group 'akn
  (akn/mode-set +vlf-auto-revert-tail-mode
    auto-revert-mode t)
  (auto-revert-mode (if auto-revert-mode 1 -1)))
(define-advice vlf-revert (:before-while (&rest _) +vlf--revert-tail)
  (or (not +vlf-auto-revert-tail-mode)
      (eq vlf-end-pos vlf-file-size)))
