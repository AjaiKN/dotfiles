;;; ui/read-aloud/config.el -*- lexical-binding: t; -*-

(require 'akn)

(defvar +read-aloud--original-input-buffer nil)
(defvar +read-aloud--pos)
(defvar +read-aloud--process nil)
(defvar +read-aloud--output-buffer-name " *say*")
(defvar +read-aloud--word-overlay nil)

(defface +read-aloud--word-face '((t :inverse-video t))
  "For highlighting the word that is being read")

(defun +read-aloud/toggle ()
  (interactive)
  (or (+read-aloud/stop)
      (+read-aloud/start)))

(defun +read-aloud/start ()
  (interactive)
  (+read-aloud/stop)
  (setq +read-aloud--original-input-buffer (current-buffer))
  ;; (push-mark)
  ;; (activate-mark)
  (with-environment-variables (("TERM" "xterm"))
    (setq +read-aloud--process
          (akn/run-command (expand-file-name "say.zsh" (dir!))
                           :name "saying"
                           :output-buffer " *say*"
                           :on-output (lambda (out)
                                        (with-current-buffer +read-aloud--original-input-buffer
                                          (dolist (line (string-split out "\n"))
                                            (let ((pos (point)))
                                              (setq line (string-trim line))
                                              (when (and (overlayp +read-aloud--word-overlay)
                                                         (eq (overlay-buffer +read-aloud--word-overlay) (current-buffer)))
                                                (goto-char (overlay-start +read-aloud--word-overlay)))
                                              (save-excursion
                                                (unless (string-empty-p line)
                                                  (when (search-forward line nil 'noerror)
                                                    (+read-aloud--remove-word-overlay)
                                                    (setq +read-aloud--word-overlay (make-overlay (match-beginning 0) (match-end 0)))
                                                    ;; (overlay-put +read-aloud--word-overlay 'face '+read-aloud--word-face)
                                                    (setq pos (match-end 0)))))
                                              (goto-char pos)))))
                           :input
                           (akn/->> (buffer-substring-no-properties (point) (point-max))
                                    (string-replace "-\n" "​") ; U+200B ZERO WIDTH SPACE
                                    (string-replace ")" "\n)")
                                    (replace-regexp-in-string (rx (group letter) (group digit))
                                                              (rx (backref 1) " " (backref 2))))
                           :boring t))))

(add-hook 'doom-escape-hook #'+read-aloud/stop)
(defun +read-aloud/stop ()
  (interactive)
  (prog1
      (when (or (and (processp +read-aloud--process)
                     (process-live-p +read-aloud--process))
                (and (get-buffer " *say*")
                     (buffer-live-p (get-buffer +read-aloud--output-buffer-name))
                     (setq +read-aloud--process (get-buffer-process (get-buffer +read-aloud--output-buffer-name)))
                     (processp +read-aloud--process)
                     (process-live-p +read-aloud--process)))
        ;; (when (overlayp +read-aloud--word-overlay)
        ;;   (with-current-buffer (overlay-buffer +read-aloud--word-overlay)
        ;;     (goto-char (overlay-start +read-aloud--word-overlay))))
        (kill-process +read-aloud--process)
        t)
    (+read-aloud--remove-word-overlay)))

(defun +read-aloud--remove-word-overlay ()
  (when (overlayp +read-aloud--word-overlay)
    (delete-overlay +read-aloud--word-overlay))
  (setq +read-aloud--word-overlay nil))
