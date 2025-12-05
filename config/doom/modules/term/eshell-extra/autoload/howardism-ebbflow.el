;;; term/eshell-extra/autoload/howardism-ebbflow.el -*- lexical-binding: t; -*-

;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org#ebb-and-flow-output-to-emacs-buffers (CC0)

(require 'dash)
(require 's)
(require 'seq)

(defvar ha-eshell-ebbflow-buffername "*eshell-edit*"
  "The name of the buffer that eshell can use to store temporary input/output.")

(defun ha-eshell-ebbflow-return ()
  "Close the ebb-flow window and return to Eshell session."
  (interactive)
  (if (and (boundp 'ha-eshell-ebbflow-return-buffer)
           (bufferp 'ha-eshell-ebbflow-return-buffer))
      (pop-to-buffer ha-eshell-ebbflow-return-buffer)
    (bury-buffer)))

(define-minor-mode ebbflow-mode
  "Editing a flow from the Eshell ebb command, so flow can pull it back."
  :lighter " ebb"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-q") 'ha-eshell-ebbflow-return)
            map))

(when (fboundp 'evil-define-key)
  (evil-define-key 'normal ebbflow-mode-map "Q" 'ha-eshell-ebbflow-return))

;;;###autoload
(defun eshell/flow (&rest args)
  "Output the contents of one or more buffers as a string.
Usage: flow [OPTION] [BUFFER ...]
    -h, --help           show this usage screen
    -l, --lines          output contents as a list of lines
    -w, --words          output contents as a list of space-separated elements "
  (let* ((options (eshell-getopts '((:name words  :short "w" :long "words")
                                    (:name lines  :short "l" :long "lines")
                                    (:name string :short "s" :long "string")
                                    (:name help   :short "h" :long "help"
                                           :help eshell/flow))
                                  args))
         (buffers (gethash 'parameters options))
         (content (thread-last buffers
                               (-map 'eshell-flow-buffer-contents)
                               (s-join "\n"))))
    (if (gethash 'help options)
        (error (documentation 'eshell/flow))

      ;; No buffer specified? Use the default buffer's contents:
      (unless buffers
        (setq content
              (eshell-flow-buffer-contents ha-eshell-ebbflow-buffername)))

      ;; Do we need to convert the output to lines or split on words?
      (cond
       ((gethash 'words options) (split-string content))
       ((gethash 'lines options) (split-string content "\n"))
       (t                        content)))))

(defun eshell-flow-buffer-contents (buffer-name)
  "Return the contents of BUFFER as a string."
  (when buffer-name
    (save-window-excursion
      (switch-to-buffer (get-buffer buffer-name))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun eshell-flow-buffers (buffers)
  "Convert the list, BUFFERS, to actual buffers if given buffer names."
  (if buffers
      (--map (cond
              ((bufferp it) it)
              ((stringp it) (get-buffer it))
              (t            (error (format "Illegal argument of type %s: %s\n%s"
                                           (type-of arg) it
                                           (documentation 'eshell/flow)))))
             buffers)
    ;; No buffers given? Use the default buffer:
    (list (get-buffer ha-eshell-ebbflow-buffername))))

;;;###autoload
(defalias 'eshell/bcat 'eshell/flow)

;;;###autoload
(defun eshell/ebb (&rest args)
  "Insert text content into *eshell-edit* buffer, or if not text is given, the output of last command.
Usage: ebb [OPTION] [text content]
    -h, --help    show this usage screen
    -m, --mode    specify the major-mode for the *eshell-edit* buffer, e.g. json
    -n, --newline separate the text contents by newlines (this is default)
    -s, --spaces  separate the text contents by spaces, instead of newlines
    -b, --begin   add text content to the beginning of the *eshell-edit* buffer
    -e, --end     add text content to the end of *eshell-edit* buffer
    -i, --insert  add text content to *eshell-edit* at point"
  (let* ((options  (eshell-getopts '((:name insert      :short "i" :long "insert")
                                     (:name append      :short "e" :long "end")
                                     (:name prepend     :short "b" :long "begin")
                                     (:name newline     :short "n" :long "newline")
                                     (:name spaces      :short "s" :long "spaces")
                                     (:name mode-option :short "m" :long "mode" :parameter string)
                                     (:name help        :short "h" :long "help"
                                            :help eshell/ebb))
                                   args))
         (location (cond
                    ((gethash 'insert  options) :insert)
                    ((gethash 'append  options) :append)
                    ((gethash 'prepend options) :prepend)
                    (t                          :replace)))
         (params   (gethash 'parameters options)))

    (if (seq-empty-p params)
        (ha-eshell-ebb-output location)
      (ha-eshell-ebb-string location (gethash 'spaces options) params))

    ;; At this point, we are in the `ha-eshell-ebbflow-buffername', and
    ;; the buffer contains the inserted data. Did we specify a major-mode?
    (when-let ((mode-option (gethash 'mode-option options)))
      (if (s-starts-with? "js" mode-option)
          (js-json-mode)  ; Or should we just go to json-ts-mode?
        (funcall (intern (concat mode-option "-mode")))))

    ;; Flip on the minor mode-option so we can close the window later on:
    (ebbflow-mode +1)
    (goto-char (point-min)))

  nil) ; Return `nil' so that it doesn't print anything in `eshell'.

(defun ha-eshell-ebb-switch-to-buffer (insert-location)
  "Switch to `ha-eshell-ebbflow-buffername' and get the buffer ready for new data."
  (let ((return-buffer (current-buffer)))

    (if-let ((ebbwindow (get-buffer-window ha-eshell-ebbflow-buffername)))
        (select-window ebbwindow)
      (switch-to-buffer ha-eshell-ebbflow-buffername)
      (setq-local ha-eshell-ebbflow-close-window t))

    (setq-local ha-eshell-ebbflow-return-buffer return-buffer)
    (ebbflow-mode)

    (cl-case insert-location
      (:append  (goto-char (point-max)))
      (:prepend (goto-char (point-min)))
      (:insert   nil)
      (:replace (delete-region (point-min) (point-max))))))

(defun ha-eshell-ebb-string (insert-location space-separator-p command-results)
  "Insert the COMMAND-RESULTS into the `ha-eshell-ebbflow-buffername`.
Contents are placed based on INSERT-LOCATION and, if given, separated
by SEPARATOR (which defaults to a space)."
  (let* ((sep (if space-separator-p " " "\n"))
         (str (string-join (-flatten command-results) sep)))
    (ha-eshell-ebb-switch-to-buffer insert-location)
    (insert str)))

(defun ha-eshell-ebb-command (insert-location command-parts)
  "Call `eshell-command' with the COMMAND-PARTS.
Inserts the output into `ha-eshell-ebbflow-buffername'"
  (let ((command-string (string-join command-parts " ")))
    (ha-eshell-ebb-switch-to-buffer insert-location)
    (eshell-command command-string t)))

(defun ha-eshell-ebb-files (insert-location files)
  "Insert the FILES at the INSERT-LOCATION tin `ha-eshell-ebbflow-buffername'."
  (ha-eshell-ebb-switch-to-buffer insert-location)
  (dolist (file files)
    (insert-file file)
    (insert "\n")))

(defun ha-eshell-last-output ()
  "Return contents of the last command execusion in an Eshell buffer."
  (let ((start  (save-excursion
                   (goto-char eshell-last-output-start)
                   (re-search-backward eshell-prompt-regexp)
                   (forward-line)
                   (line-beginning-position)))
        (end    eshell-last-output-start))
    (buffer-substring-no-properties start end)))

(defun ha-eshell-ebb-output (insert-location)
  "Grab output from previous eshell command, inserting it into our buffer.
Gives the INSERT-LOCATION to `ha-eshell-ebb-switch-to-buffer'."
  (let ((contents (ha-eshell-last-output)))
    (ha-eshell-ebb-switch-to-buffer insert-location)
    (insert contents)))
