;; -*- lexical-binding: t; -*-

;; https://gitlab.com/akashadutchie/sly-mrepl-db/-/blob/fc2894c60c07b0746d55201a188446d78753c76c/sly-mrepl-sly-db.el
;; Author: Akasha Peppermint
;; (no license)

(require 'sly)
(require 'sly-mrepl)

(sly-make-variables-buffer-local
 (defvar sly-mrepl-db-thread nil
   "thread for a sly-mrepl-db")

 (defvar sly-mrepl-db-frame nil)
 (defvar sly-mrepl-db-package nil)

 (defvar sly-mrepl-is-db-mrepl nil)
 (defvar sly-mrepl-db-sly-db-emitter nil)
 ;; (defvar sly-db-sl(sly-db-mrepl-find-or-create-buffer db-buffer)y-db-mrepl-emitter nil)

 (defvar sly-mrepl-db-normal-mrepl-fn 'sly-mrepl--input-sender
   "Replaced with sly-mrepl-db--input-sender"))

;;
(defun sly-db-mrepl--insert-note (string &optional face)
  (let* ((face (or face 'sly-mrepl-note-face)))
    (cond ((sly-mrepl--process)
           ;; notes are inserted "synchronously" with the process mark  process
           (sly-mrepl--ensure-newline)
           (sly-mrepl--insert ";" face)
           (sly-mrepl--insert string face)
           (sly-mrepl--ensure-newline))
          (t
           ;; If no process yet, fall back to the simpler strategy.
           (sly-mrepl--insert-output string face)))))

;; (defun sly-mrepl-db-eval (string frame package)
;;   )

(defun sly-mrepl-db-eval-in-frame (string)
  ;; For some reason its trying to dispatch this in a wrapped
  ;; (sly-mrepl--send `((:db-eval-in-frame ,sly-mrepl-db-frame ,string ,sly-mrepl-db-package)))

  ;; (sly-mrepl--send-string)
  ;; This is the code i want run, but we are in the MREPL and there are vars shadowed
  ;; (sly-db-eval-in-frame sly-mrepl-db-frame string sly-mrepl-db-package)
  (if (and sly-mrepl-db-sly-db-emitter
           (get-buffer sly-mrepl-db-sly-db-emitter))
      (let* ((frame sly-mrepl-db-frame)
             (package sly-mrepl-db-package)
             ;; (buffer (buffer-name (current-buffer)))
             (string (if (equal "" string) "nil" string))
             (cb (lambda (x)
                   (sly-display-eval-result x)
                   (with-current-buffer ;sly-db-sly-db-mrepl-emitter
                       (sly-db-mrepl-find-or-create-buffer (current-buffer))
                     ;; Works but i would like it a button
                     ;; (sly-mrepl--insert-output
                     ;;  x
                     ;;  'sly-mrepl-output-face t)
                     ;; (sly-mrepl--insert x nil)

                     (if (null x)
                         (sly-mrepl--insert-note "No values")
                       ;; doesnt work for some reason
                       ;; (sly-mrepl--insert
                       ;;  (sly-mrepl--make-result-button result 0))

                       (sly-mrepl--insert
                        (with-current-buffer sly-mrepl-db-sly-db-emitter
                          (sly--make-text-button x nil
                                                 :type 'sly-db-mrepl-local-variable
                                                 'part-args (list 0 0)
                                                 'part-label (format "Local Variable %d" 0)))))
                     (sly-mrepl--ensure-newline)))))
        (with-current-buffer sly-mrepl-db-sly-db-emitter
          ;; (sly-db-eval-in-frame frame string package)
          (message "@@")
          ;; (message sly-db-sly-db-mrepl-emitter)
          (message string)
          (message (pp-to-string frame))
          (message package)
          (message "$$")
          (sly-eval-async `(slynk:eval-string-in-frame
                            ,string
                            ,frame
                            ,package)
            cb)))

    (progn
      (sly-db-mrepl--insert-note
       "Sly-DB buffer killed, nowhere to send command."))))

(defun sly-mrepl-db--input-sender (_proc string)
  (sly-mrepl-db-eval-in-frame (substring-no-properties string)))

(defun sly-mrepl-db-return (&optional end-of-input)
  (interactive "P")
  (cl-assert (sly-connection))
  (cl-assert (process-live-p (sly-mrepl--process)) nil
             "No local live process, cannot use this REPL")
  (cond
   ;; ((and
   ;;   (not sly-mrepl--read-mark)
   ;;   (sly-mrepl--busy-p))
   ;;  (sly-message "REPL is busy"))
   ((and (not sly-mrepl--read-mark)
         (or (sly-input-complete-p (sly-mrepl--mark) (point-max))
             end-of-input))
    (sly-mrepl--send-input-sexp)
    (sly-mrepl--catch-up))
   (sly-mrepl--read-mark
    (unless end-of-input
      (goto-char (point-max))
      (newline))
    (let ((comint-input-filter (lambda (_s) nil)))
      (comint-send-input 'no-newline))
    (sly-mrepl--catch-up))
   (t
    (newline-and-indent)
    (sly-message "Input not complete"))))

(defvar sly-mrepl-db-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'sly-mrepl-db-return)
    map)
  "sly-mrepl-db-minor-mode keymap.")

(define-minor-mode sly-mrepl-db-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :keymap sly-mrepl-db-minor-mode-map)

(defun sly-db-mrepl-default-prompt (_package
                                    nickname
                                    error-level
                                    _entry-idx
                                    _condition)
  (concat
   (when (cl-plusp error-level)
     (concat (sly-make-action-button
              (format "[%d]" error-level)
              #'sly-db-pop-to-debugger-maybe)
             " "))
   (propertize
    (format "[sly] Eval in frame %s of %s > " (pp-to-string sly-mrepl-db-frame) (pp-to-string nickname))
    'face 'sly-mrepl-prompt-face
    'font-lock-face 'sly-mrepl-prompt-face)))

(defun sly-db-mrepl-find-or-create-buffer (sly-db-buffer)
  (message (buffer-name (get-buffer (sly-mrepl--buffer-name (sly-connection)
                                                            (format "sly-db for %s" (buffer-name))))))
  (let* ((_connection (let ((sly-buffer-connection nil)
                            (sly-dispatching-connection nil))
                        (sly-connection)))
         (dbuf-name (buffer-name sly-db-buffer))
         ;; (bufname (format "*sly-db mrepl for %s*" (buffer-name)))
         (buffer (or
                  ;; Doesn't work if emacs suffixes buffer name with <#> s
                  ;; (get-buffer (sly-mrepl--buffer-name connection
                  ;;                                     (format "sly-db for %s" (buffer-name))))
                  ;; (message (sly-mrepl--buffer-name connection
                  ;;                                  (format "sly-db for %s" (buffer-name))))
                  (cl-find-if
                   (lambda (x)
                     (with-current-buffer x
                       ;; check some local vars to see if it matches
                       (equal sly-mrepl-db-sly-db-emitter dbuf-name)))
                   (buffer-list))

                  ;; copypasta
                  ;; (car (cl-sort (cl-remove-if-not
                  ;;                (lambda (x)
                  ;;                  (with-current-buffer x
                  ;;                    (and sly-mrepl-is-db-mrepl
                  ;;                         (eq major-mode 'sly-mrepl-mode)
                  ;;                         (eq sly-buffer-connection connection)
                  ;;                         (or (not thread)
                  ;;                             (eq thread sly-current-thread)))))
                  ;;                (buffer-list)) #'< :key (sly-compose #'length #'buffer-name)))
                  (sly-mrepl-new (sly-connection) ;"sly-db";(format "*sly-db mrepl for %s*" (buffer-name))
                                 (format "sly-db for %s" (buffer-name))))))
    ;; put elsewhere
    (with-current-buffer buffer
      (sly-mrepl-db-minor-mode 1)
      (setq-local sly-mrepl-db-normal-mrepl-fn (or comint-input-sender sly-mrepl-db-normal-mrepl-fn)
                  sly-mrepl-prompt-formatter #'sly-db-mrepl-default-prompt
                  sly-mrepl-is-db-mrepl t
                  sly-mrepl-db-sly-db-emitter (buffer-name sly-db-buffer))
      (setf ;; comint-input-sender #'sly-mrepl--input-sender
            comint-input-sender 'sly-mrepl-db--input-sender))

    (buffer-name buffer)))

(defun sly-db-mrepl-push-db-button (frame-id var-id)
  (with-current-buffer sly-mrepl-db-sly-db-emitter
    (sly-eval-for-inspector `(slynk:inspect-frame-var ,frame-id
                              ,var-id))))
;; (defun sly-db-mrepl-push-db-button1 (button)
;;   (let ((frame-id (button-get frame-button 'frame-number))
;;         (var-id (button-get frame-button 'variable-name)))
;;     (with-current-buffer sly-mrepl-db-sly-db-emitter
;;       (sly-eval-for-inspector `(slynk:inspect-frame-var ,frame-id
;;                                                         ,var-id)))))

(define-button-type 'sly-db-mrepl-local-variable :supertype 'sly-db-local-variable
  'sly-button-inspect
  #'sly-db-mrepl-push-db-button)

(defun sly-db-mrepl-prompt-in-frame (frame-button)
  "Show details for FRAME-BUTTON"
  (interactive (list (sly-db-frame-button-near-point)))

  ;; Print locals and catch tags in the db-mrepl
  (let* ((db-buffer (current-buffer))
         (frame-number (button-get frame-button 'frame-number))
         (package (sly-eval `(slynk:frame-package-name ,frame-number)))
         (dbmrepl-buffer (sly-db-mrepl-find-or-create-buffer db-buffer)))

    (with-current-buffer db-buffer
      (cl-destructuring-bind (locals _catches)
          (sly-eval `(slynk:frame-locals-and-catch-tags ,frame-number))

        ;; doesn't work because sly nukes its buffers appropriately.
        ;; (setf sly-db-sly-db-mrepl-emitter dbmrepl-buffer)
        (with-current-buffer dbmrepl-buffer

          (setf sly-mrepl-db-frame frame-number
                sly-mrepl-db-package package)

          (sly-db-mrepl--insert-note
           (format "Frame %s of Package %s" frame-number package))

          (sly-db-mrepl--insert-note
           (sly-db-in-face section (if locals "Locals:" "[No Locals]"))
           'sly-db-section-face)

          (cl-loop for i from 0
                   for var in locals
                   do
                   (cl-destructuring-bind (&key name id value) var
                     (sly-mrepl--ensure-newline)
                     (sly-mrepl--insert (concat ";" name (if (zerop id)
                                                             ""
                                                           (format "#%d" id))) 'sly-db-local-name-face)
                     (sly-mrepl--insert " = ")
                     (sly-mrepl--insert
                      (with-current-buffer db-buffer
                        (sly--make-text-button value nil
                                               :type 'sly-db-mrepl-local-variable
                                               'part-args (list frame-number i)
                                               'part-label (format "Local Variable %d" i))))
                     (sly-mrepl--ensure-newline)))
          ;; (goto-char (point-max))
          (set-window-point (get-buffer-window) (point-max)))))))
        ;; (sly-recenter (field-end (button-start frame-button) 'escape))

  ;; Print prompt to mrepl


(defun sly-db-kill-associated-mrepl-h ()
  (when-let* ((buffer (let* ((sly-db-buffer (current-buffer))
                             (dbuf-name (buffer-name sly-db-buffer)))
                        (cl-find-if
                         (lambda (x)
                           (with-current-buffer x
                             ;; check some local vars to see if it matches
                             (equal sly-mrepl-db-sly-db-emitter dbuf-name)))
                         (buffer-list)))))
    (kill-buffer buffer)))

(add-hook! 'sly-db-mode-hook
  (add-hook! 'kill-buffer-hook :local #'sly-db-kill-associated-mrepl-h))


(with-eval-after-load 'sly
  (define-key sly-db-frame-map "z" 'sly-db-mrepl-prompt-in-frame))

(provide 'sly-mrepl-sly-db)
