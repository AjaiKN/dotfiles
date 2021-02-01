;;; term/mistty/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+mistty/toggle #'mistty-in-project)

;;;###autoload
(defun +mistty/here (&optional use-existing-p)
  (interactive "P")
  (let (display-buffer-alist
        (current-prefix-arg (not use-existing-p)))
    (mistty-in-project)))

;;;; other shells
;;;###autoload
(defun +mistty/shell (command)
  (interactive (list (completing-read "shell: " '("zsh" "fish" "bash" "nu" "csh" "pwsh" "sh" "tcsh" "dash" "ksh"))))
  (mistty-create command))

;;;###autoload
(defun +mistty/fish ()
  (interactive)
  (mistty-create "fish"))

;;;; C-d
(defvar +mistty--buf-to-close)

;;;###autoload
(defun +mistty--done-buffer-p (buffer)
  (and buffer
       (buffer-live-p buffer)
       (with-current-buffer buffer (derived-mode-p 'mistty-mode))
       (not (mistty-live-buffer-p buffer))
       buffer))

;;;###autoload
(defun +mistty/C-d (&optional n)
  (interactive "p" mistty-mode)
  (mistty-send-key n (kbd "C-d"))
  (setq +mistty--buf-to-close (current-buffer))
  (akn/after-idle! (0.00) (when (+mistty--done-buffer-p +mistty--buf-to-close) (kill-buffer +mistty--buf-to-close) (doom-log "0")))
  (akn/after-idle! (0.05) (when (+mistty--done-buffer-p +mistty--buf-to-close) (kill-buffer +mistty--buf-to-close) (doom-log ".05")))
  (akn/after-idle! (0.10) (when (+mistty--done-buffer-p +mistty--buf-to-close) (kill-buffer +mistty--buf-to-close) (doom-log ".1")))
  (akn/after-idle! (0.20) (when (+mistty--done-buffer-p +mistty--buf-to-close) (kill-buffer +mistty--buf-to-close) (doom-log ".2")))
  (akn/after-idle! (0.50) (when (+mistty--done-buffer-p +mistty--buf-to-close) (kill-buffer +mistty--buf-to-close) (doom-log ".5")))
  (akn/after-idle! (2.00) (when (+mistty--done-buffer-p +mistty--buf-to-close) (kill-buffer +mistty--buf-to-close) (doom-log "1"))))

;;;; shift tab

(defun +mistty/shift-tab-command (&optional n)
  "Send a shift-tab to the terminal.

If N is specified, do it N times."
  (interactive "p" mistty-mode)
  ;; (dotimes (_ n)
  ;;   (mistty-send-key 1 (kbd "ESC") 'positional)
  ;;   (mistty-send-key 1 (kbd "[") 'positional)
  ;;   (mistty-send-key 1 (kbd "Z") 'positional)))
  (mistty-send-key n (kbd "<backtab>") 'positional))

;;;; prompt fields
;;;###autoload
(defun +mistty--make-prompt-field-a (&rest _)
  (message "called")
  (let ((inhibit-read-only t))
    ;; (mistty--with-live-buffer mistty-work-buffer
    (when-let* ((end (+mistty--input-start))
                (beg (save-excursion (goto-char end) (beginning-of-line) (point))))
      (remove-text-properties (pos-bol) (point-max) '(read-only t field +mistty-prompt front-sticky t rear-nonsticky t))
      (message "marking between %s and %s" beg end)
      (add-text-properties beg end '(read-only t field +mistty-prompt front-sticky t rear-nonsticky t)))))

(defun +mistty--remove-prompt-field-a (&rest _)
  (let ((inhibit-read-only t))
    (remove-text-properties (pos-bol) (point-max) '(read-only t field +mistty-prompt front-sticky t rear-nonsticky t))))

;;;; corfu
;;;###autoload
(defun +mistty--turn-off-corfu-mode ()
  (corfu-mode -1))

;;;; setting cursor correctly
;;;###autoload
(defun mistty--set-cursor-right-a ()
  :after #'mistty--post-command
  (when (eq cursor-type t)
    (setq cursor-type (pcase evil-state
                        ('insert evil-insert-state-cursor)
                        ('normal evil-normal-state-cursor)
                        ('operator evil-operator-state-cursor)
                        ('visual evil-visual-state-cursor)
                        ('motion evil-motion-state-cursor)
                        ('replace evil-replace-state-cursor)
                        ('emacs evil-emacs-state-cursor)))))

;;;; strict mouse mode
;;;###autoload
(define-minor-mode +mistty-strict-mouse-mode
  "When clicking on a mistty, immediately move the cursor back to where it
belongs according to the underlying terminal."
  :group 'akn
  (when +mistty-strict-mouse-mode
    (advice-add #'mouse-set-point :after #'+mistty--reset-cursor-point-a)))

;;;###autoload
(defun +mistty--reset-cursor-point-a (&rest _args)
  (when (and +mistty-strict-mouse-mode
             (> (point) (mistty-cursor)))
    (mistty-goto-cursor)))

;;;; helper functions for input
;;;###autoload
(defun +mistty--input-start ()
  (or ;(cadr mistty--possible-prompt)
      (and (thing-at-point 'line)
           (string-match (rx bol
                             (* (not (any "#$%>»❯▶❮V" "\\n")))
                             (+ (any "#$%>»❯▶❮V"))
                             " ")
                         (thing-at-point 'line))
           (match-end 0)
           (save-excursion (beginning-of-line)
                           (right-char (match-end 0))
                           (point)))))

;;;###autoload
(defun +mistty--in-input-p ()
  (let ((input-start (+mistty--input-start)))
    (and input-start
         (>= (point) input-start))))

;;;; narrowing to input
;;;###autoload
(defun +mistty--narrow-to-input-a (func &rest args)
  "Run FUNC with ARGS, with buffer narrowed to after the prompt."
  (let ((input-start))
    (if (and (derived-mode-p 'mistty-mode)
             (mistty-on-prompt-p (point))
             (setq input-start (+mistty--input-start))
             (>= (point) input-start))
        (save-restriction
          (narrow-to-region input-start (point-max))
          (apply func args))
      (apply func args))))

(defun +mistty-narrow-to-input-h ()
  "Narrow buffer to after the prompt."
  (let ((input-start)
        (mistty-buffer (current-buffer)))
        ;; (mistty-buffer (current-buffer)))
    (when (and (not (memq this-command +mistty-widened-commands))
               (derived-mode-p 'mistty-mode)
               (mistty-on-prompt-p (point))
               (setq input-start (+mistty--input-start))
               (>= (point) input-start))
      (add-transient-hook! 'post-command-hook
        (with-current-buffer mistty-buffer
          (+mistty-widen-h)))
      (narrow-to-region input-start (point-max)))))
      ;; (add-transient-hook! 'post-command-hook
      ;;   (with-current-buffer mistty-buffer
      ;;     (widen))))))

(defun +mistty-widen-h ()
  (when (derived-mode-p 'mistty-mode)
    (widen)))
