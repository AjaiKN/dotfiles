;;; ui/tabs/autoload.el -*- lexical-binding: t; -*-

(require 'akn)

(defvar +tabs--line-scroll-count 0)
;;;###autoload
(defun +tabs/hscroll-left (&rest args)
  (interactive (list current-prefix-arg last-nonmenu-event))
  (cl-incf +tabs--line-scroll-count)
  (when (zerop (mod +tabs--line-scroll-count 3))
    (apply #'tab-line-hscroll-left args)
    (setq +tabs--line-scroll-count 0)))
;;;###autoload
(defun +tabs/hscroll-right (&rest args)
  (interactive (list current-prefix-arg last-nonmenu-event))
  (cl-incf +tabs--line-scroll-count)
  (when (zerop (mod +tabs--line-scroll-count 3))
    (apply #'tab-line-hscroll-right args)
    (setq +tabs--line-scroll-count 0)))


(defvar-local +tabs-show-modified-star t)

;;;###autoload
(defun +tabs-name-fn (buffer &optional _buffers)
  (let ((name (buffer-name buffer)))
    (when (akn/polymode-p buffer)
      (setq buffer (or (buffer-base-buffer buffer) buffer))
      (setq name (replace-regexp-in-string (rx bol (+ " ")) "" (buffer-name buffer))))
    (concat name
            (when (and (buffer-file-name buffer)
                       (buffer-modified-p buffer)
                       (buffer-local-value '+tabs-show-modified-star buffer))
              " (*)"))))

;;;###autoload
(defun +tabs-fn ()
  (let ((workspace-buffers (cons (current-buffer) (+tabs--workspace-buffer-list))))
    (cl-loop for b in (tab-line-tabs-window-buffers)
             for base-buffer = (if (akn/polymode-p b)
                                   (or (buffer-base-buffer b) b)
                                 b)
             if (and (or (doom-real-buffer-p base-buffer) (akn/polymode-p base-buffer))
                     (or (memq base-buffer workspace-buffers) (memq b workspace-buffers)))
             collect b)))

(defun akn/terminal-buffers ()
  (seq-sort-by #'buffer-name #'string<
               (seq-filter #'akn/terminal-buffer-p
                           (+tabs--workspace-buffer-list))))

(defun akn/help-tabs (&rest _)
  (append (seq-filter #'buffer-live-p
                      (append
                       (match-buffers '(derived-mode . Info-mode))
                       (match-buffers (rx "*Help*"))))
          (and (boundp '+emacs-lisp--helpful-buffer-ring)
               (nreverse (ring-elements +emacs-lisp--helpful-buffer-ring)))))

;;;###autoload
(defun +tabs-maybe-add-tab-line-for-popup-buffer ()
  (when (+popup-buffer-p)
    (cond
     ((derived-mode-p 'helpful-mode 'help-mode 'Info-mode)
      (setq-local tab-line-tabs-function #'akn/help-tabs)
      (tab-line-mode))
     ((akn/terminal-buffer-p)
      (when (fboundp #'persp-add-buffer) (persp-add-buffer (current-buffer)))
      (setq-local tab-line-tabs-function #'akn/terminal-buffers)
      (tab-line-mode))
     (t (tab-line-mode -1)))))

(defun +tabs--workspace-buffer-list ()
  (if (fboundp '+workspace-buffer-list)
      (+workspace-buffer-list)
    (buffer-list)))
