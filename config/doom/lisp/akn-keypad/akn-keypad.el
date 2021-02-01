;;; akn-keypad.el --- keypad mode based on meow-keypad (WIP, not working well) -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This was extracted and adapted from meow-keypad:
;;   https://github.com/meow-edit/meow/blob/4ab409f51b3fe6306edd48135f455646ae2a9a42/meow-keypad.el
;;
;; Keypad state is a special state to simulate C-x and C-c key sequences.
;; There are three commands:
;;
;; akn-keypad-start
;; Enter keypad state, and simulate this key with Control modifier.
;;
;; akn-keypad-self-insert
;; This command is bound to every single key in keypad state.
;; The rules,
;; - If current key is SPC, the next will be considered without modifier.
;; - If current key is m, the next will be considered with Meta modifier.
;; - Other keys, or SPC and m after a prefix, means append a key input, by default, with Control modifier.
;;
;; akn-keypad-undo
;; Remove the last input, if there's no input in the sequence, exit the keypad state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Vars

(defgroup akn-keypad nil
  "Custom group for akn-keypad."
  :group 'emacs)

(defvar akn-keypad--this-command nil
  "Command name for current keypad execution.")

(defvar akn-keypad--keymap-description-activated nil
  "Whether KEYPAD keymap description is already activated.")

(defvar akn-keypad--help nil
  "If keypad in help mode.")

(defvar akn-keypad--base-keymap nil
  "The keymap used to lookup keys in KEYPAD state.

Nil means to lookup in top-level.")

(defcustom akn-keypad-message t
  "Whether to log keypad messages in minibuffer."
  :group 'akn-keypad
  :type 'boolean)

(defcustom akn-keypad-self-insert-undefined nil
  "Whether to self-insert a key in keypad mode if it is undefined"
  :group 'akn-keypad
  :type 'boolean)

(defcustom akn-keypad-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'akn-keypad
  :type 'number)

(defvar akn-keypad-state-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] #'akn-keypad-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) #'akn-keypad-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL")           #'akn-keypad-undo)
      (define-key map (kbd "<backspace>")   #'akn-keypad-undo)
      (define-key map (kbd "<escape>")      #'akn-keypad-quit)
      (define-key map [remap keyboard-quit] #'akn-keypad-quit)
      (define-key map (kbd "<deletechar>")  #'akn-keypad-self-insert)
      (define-key map (kbd "<tab>")         #'akn-keypad-self-insert)
      (define-key map (kbd "TAB")           #'akn-keypad-self-insert)
      (define-key map (kbd "<return>")      #'akn-keypad-self-insert)
      (define-key map (kbd "<up>")          #'akn-keypad-self-insert)
      (define-key map (kbd "<down>")        #'akn-keypad-self-insert)
      (define-key map (kbd "<left>")        #'akn-keypad-self-insert)
      (define-key map (kbd "<right>")       #'akn-keypad-self-insert)
      (define-key map (kbd "<home>")        #'akn-keypad-self-insert)
      (define-key map (kbd "<end>")         #'akn-keypad-self-insert)
      (define-key map (kbd "<next>")        #'akn-keypad-self-insert)
      (define-key map (kbd "<prior>")       #'akn-keypad-self-insert)
      (define-key map (kbd "<insert>")      #'akn-keypad-self-insert)
      (define-key map (kbd "RET")           #'akn-keypad-self-insert))
    map)
  "Keymap for Akn keypad state.")

(defcustom akn-keypad-leader-dispatch mode-specific-map
  "The fallback dispatching in KEYPAD when there's no translation.

The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for using `mode-specific-map' (C-c map)."
  :group 'akn-keypad
  :type '(choice (string :tag "Keys")
                 (variable :tag "Keymap")
                 (const nil)))

(defcustom akn-keypad-meta-prefix ?m
  "The prefix represent M- in KEYPAD state."
  :group 'akn-keypad
  :type 'character)

(defcustom akn-keypad-ctrl-meta-prefix ?g
  "The prefix represent C-M- in KEYPAD state."
  :group 'akn-keypad
  :type 'character)

(defcustom akn-keypad-literal-prefix 32
  "The prefix represent no modifier in KEYPAD state."
  :group 'akn-keypad
  :type 'character)

(defcustom akn-keypad-start-keys
  '((?c . ?c)
    (?h . ?h)
    (?x . ?x))
  "Alist of keys to begin keypad translation. When a key char is pressed,
it's corresponding value is appended to C- and the user is
prompted to finish the command."
  :group 'akn-keypad
  :type '(alist :key-type (character :tag "From")
                :value-type (character :tag "To")))

(defvar akn-keypad-describe-keymap-function #'akn-keypad-describe-keymap
  "The function used to describe (KEYMAP) during keypad execution.

To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Akn ships a default `akn-keypad-describe-keymap'.
Use (setq akn-keypad-describe-keymap-function \\='nil) to disable popup.")

(defvar akn-keypad-get-title-function #'akn-keypad-get-title
  "The function used to get the title of a keymap or command.")

(defvar akn-keypad--keys nil)
(defvar akn-keypad--allow-quick-dispatch nil)

(defvar akn-keypad--prefix-arg nil)
(defvar akn-keypad--use-literal nil)
(defvar akn-keypad--use-meta nil)
(defvar akn-keypad--use-both nil)

(defface akn-keypad-cannot-display
  '((((class color) (background dark))
     (:height 0.7 :foreground "grey90"))
    (((class color) (background light))
     (:height 0.7 :foreground "grey10")))
  "Face for Akn keypad message when cannot display popup."
  :group 'akn-keypad)

;;;; Utils

(defun akn-keypad--event-key (e)
  (let ((c (event-basic-type e)))
    (if (and (char-or-string-p c)
             (member 'shift (event-modifiers e)))
        (upcase c)
      c)))

(defun akn-keypad--get-event-key (e)
  (if (and (integerp (event-basic-type e))
           (member 'shift (event-modifiers e)))
      (upcase (event-basic-type e))
    (event-basic-type e)))

(defun akn-keypad--transpose-lists (lists)
  (when lists
    (let* ((n (seq-max (mapcar #'length lists)))
           (rst (apply #'list (make-list n ()))))
      (mapc (lambda (l)
              (seq-map-indexed
               (lambda (it idx)
                 (cl-replace rst
                             (list (cons it (nth idx rst)))
                             :start1 idx
                             :end1 (1+ idx)))
               l))
            lists)
      (mapcar #'reverse rst))))

(defun akn-keypad--sum (sequence)
  (seq-reduce #'+ sequence 0))

(defun akn-keypad--string-join (sep s)
  (string-join s sep))

(defun akn-keypad--is-self-insertp (cmd)
  (and (symbolp cmd)
       (string-match-p "\\`.*self-insert.*\\'"
                       (symbol-name cmd))))

(defun akn-keypad--string-pad (s len pad &optional start)
  (if (<= len (length s))
      s
    (if start
        (concat (make-string (- len (length s)) pad) s)
      (concat s (make-string (- len (length s)) pad)))))

(defun akn-keypad--parse-input-event (e)
  (cond
   ((equal e 32)
    "SPC")
   ((characterp e)
    (string e))
   ((equal 'tab e)
    "TAB")
   ((equal 'return e)
    "RET")
   ((equal 'backspace e)
    "DEL")
   ((equal 'escape e)
    "ESC")
   ((symbolp e)
    (format "<%s>" e))
   (t nil)))

(defun akn-keypad--parse-string-to-keypad-keys (str)
  (let ((strs (split-string str " ")))
    (thread-last
      strs
      (mapcar
       (lambda (str)
         (cond
          ((string-prefix-p "C-M-" str)
           (cons 'both (substring str 4)))
          ((string-prefix-p "C-" str)
           (cons 'control (substring str 2)))
          ((string-prefix-p "M-" str)
           (cons 'meta (substring str 2)))
          (t
           (cons 'literal str)))))
      (reverse))))

(defun akn-keypad--get-leader-keymap ()
  (and (keymapp akn-keypad-leader-dispatch)
       akn-keypad-leader-dispatch))

;;;; which-key integration

(defvar which-key-mode)
(declare-function which-key--create-buffer-and-show "which-key"
                  (&optional prefix-keys from-keymap filter prefix-title))

(defvar akn-keypad--which-key-setup nil)

(defun akn-keypad--which-key-describe-keymap ()
  (if which-key-mode
      (setq akn-keypad-describe-keymap-function #'akn-keypad--which-key-describe-keymap-function)
    (setq akn-keypad-describe-keymap-function #'akn-keypad-describe-keymap)))

(defun akn-keypad--which-key-describe-keymap-function (keymap)
  (which-key--create-buffer-and-show nil keymap nil (concat "Keypad: " (akn-keypad--format-keys))))

(defun akn-keypad--setup-which-key (enable)
  (setq akn-keypad--which-key-setup enable)
  (if enable
      (add-hook 'which-key-mode-hook #'akn-keypad--which-key-describe-keymap)
    (remove-hook 'which-key-mode-hook #'akn-keypad--which-key-describe-keymap)))

;;;; keypad state

(define-minor-mode akn-keypad-mode
  "Minor mode for when using keypad."
  :init-value nil
  :global t
  :lighter " [K]"
  :keymap akn-keypad-state-keymap
  (if akn-keypad-mode
      (progn
        ;; (message "starting")
        (setq-default
         overriding-local-map akn-keypad-state-keymap
         overriding-terminal-local-map nil
         akn-keypad--prefix-arg current-prefix-arg
         akn-keypad--keymap-description-activated nil
         akn-keypad--allow-quick-dispatch t
         akn-keypad--base-keymap nil
         akn-keypad--keys nil
         akn-keypad--use-literal nil
         akn-keypad--use-meta nil
         akn-keypad--use-both nil))
    ;; (message "ending")
    (setq-default
     overriding-local-map nil
     akn-keypad--keymap-description-activated nil
     akn-keypad--allow-quick-dispatch nil
     akn-keypad--keys nil
     akn-keypad--use-literal nil
     akn-keypad--use-meta nil
     akn-keypad--use-both nil
     akn-keypad--help nil)))

;;;; Main stuff

(defun akn-keypad--format-upcase (k)
  "Return S-k for upcase k."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun akn-keypad--format-key-1 (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (akn-keypad--format-upcase (cdr key))))
    (both (format "C-M-%s" (akn-keypad--format-upcase (cdr key))))
    (literal (cdr key))))

(defun akn-keypad--format-prefix ()
  "Return a display format for current prefix."
  (cond
   ((equal '(4) akn-keypad--prefix-arg)
    "C-u ")
   (akn-keypad--prefix-arg
    (format "%s " akn-keypad--prefix-arg))
   (t "")))

(defun akn-keypad--lookup-key (keys)
  (let* ((overriding-local-map akn-keypad--base-keymap)
         (keybind (key-binding keys)))
    (unless (and (akn-keypad--is-self-insertp keybind)
                 (not akn-keypad-self-insert-undefined))
      keybind)))

(defun akn-keypad--has-sub-meta-keymap-p ()
  (and (not akn-keypad--use-literal)
       (not akn-keypad--use-both)
       (not akn-keypad--use-meta)
       (or (not akn-keypad--keys)
           (let* ((key-str (akn-keypad--format-keys nil))
                  (keymap (akn-keypad--lookup-key (kbd key-str))))
             (and (keymapp keymap)
                  (lookup-key keymap ""))))))

(defun akn-keypad--format-keys (&optional prompt)
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
              (mapcar #'akn-keypad--format-key-1 akn-keypad--keys)
            (reverse)
            (string-join " ")))
    (cond
     (akn-keypad--use-both
      (setq result
            (if (string-empty-p result)
                "C-M-"
              (concat result " C-M-"))))
     (akn-keypad--use-meta
      (setq result
            (if (string-empty-p result)
                "M-"
              (concat result " M-"))))
     (akn-keypad--use-literal
      (setq result (concat result " ○")))

     (prompt
      (setq result (concat result " C-"))))
    result))

(defun akn-keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when akn-keypad-message
    (message "KEYPAD exit"))
  (akn-keypad-mode -1))

(defun akn-keypad--make-keymap-for-describe (keymap control)
  (let ((km (make-keymap)))
    (suppress-keymap km t)
    (when (keymapp keymap)
      (map-keymap
       (lambda (key def)
         (unless (member (event-basic-type key) '(127))
           (when (if control
                     (member 'control (event-modifiers key))
                   (not (member 'control (event-modifiers key))))
             (define-key km (vector (akn-keypad--get-event-key key))
                         (funcall akn-keypad-get-title-function def)))))
       keymap))
    km))

(defun akn-keypad--get-keymap-for-describe ()
  (let* ((input (thread-first
                  (mapcar #'akn-keypad--format-key-1 akn-keypad--keys)
                  (reverse)
                  (string-join " ")))
         (meta-both-keymap (akn-keypad--lookup-key
                            (read-kbd-macro
                             (if (string-blank-p input)
                                 "ESC"
                               (concat input " ESC"))))))
    (cond
     (akn-keypad--use-meta
      (when meta-both-keymap
        (akn-keypad--make-keymap-for-describe meta-both-keymap nil)))
     (akn-keypad--use-both
      (when meta-both-keymap
        (akn-keypad--make-keymap-for-describe meta-both-keymap t)))
     (akn-keypad--use-literal
      (when-let ((keymap (akn-keypad--lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (akn-keypad--make-keymap-for-describe keymap nil))))

     ;; For leader popup
     ;; akn-keypad-leader-dispatch can be string, keymap or nil
     ;; - string, dynamically find the keymap
     ;; - keymap, just use it
     ;; - nil, use mode-specific-map (C-c map)
     ;; Leader keymap may contain akn-keypad-dispatch commands
     ;; translated names based on the commands they refer to
     ((null akn-keypad--keys)
      (when-let ((keymap (if (stringp akn-keypad-leader-dispatch)
                             (akn-keypad--lookup-key (read-kbd-macro akn-keypad-leader-dispatch))
                           (or akn-keypad-leader-dispatch
                               mode-specific-map))))
        (let ((km (make-keymap)))
          (suppress-keymap km t)
          (map-keymap
           (lambda (key def)
             (when (and (not (member 'control (event-modifiers key)))
                        (not (member key (list akn-keypad-meta-prefix
                                               akn-keypad-ctrl-meta-prefix
                                               akn-keypad-literal-prefix)))
                        (not (alist-get key akn-keypad-start-keys)))
               (let ((keys (vector (akn-keypad--get-event-key key))))
                 (unless (lookup-key km keys)
                   (define-key km keys (funcall akn-keypad-get-title-function def))))))
           keymap)
          km)))

     (t
      (when-let ((keymap (akn-keypad--lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (let* ((km (make-keymap))
                 (has-sub-meta (akn-keypad--has-sub-meta-keymap-p))
                 (ignores (if has-sub-meta
                              (list akn-keypad-meta-prefix
                                    akn-keypad-ctrl-meta-prefix
                                    akn-keypad-literal-prefix
                                    127)
                            (list akn-keypad-literal-prefix 127))))
            (suppress-keymap km t)
            (map-keymap
             (lambda (key def)
               (when (member 'control (event-modifiers key))
                 (unless (member (akn-keypad--event-key key) ignores)
                   (when def
                     (let ((k (vector (akn-keypad--get-event-key key))))
                       (unless (lookup-key km k)
                         (define-key km k (funcall akn-keypad-get-title-function def))))))))
             keymap)
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (unless (member key ignores)
                   (let ((k (vector (akn-keypad--get-event-key key))))
                     (unless (lookup-key km k)
                       (define-key km (vector (akn-keypad--get-event-key key)) (funcall akn-keypad-get-title-function def)))))))
             keymap)
            km)))))))

(defun akn-keypad--display-message ()
  (let (overriding-local-map)
    (when akn-keypad-describe-keymap-function
      (when (or
             akn-keypad--keymap-description-activated
             (progn
               ;; (message "sitting")
               (setq akn-keypad--keymap-description-activated
                     (sit-for akn-keypad-describe-delay t))))
        ;; (message "describing")
        (let ((keymap (akn-keypad--get-keymap-for-describe)))
          (funcall akn-keypad-describe-keymap-function keymap))))))

(defun akn-keypad--describe-keymap-format (pairs &optional width)
  (let* ((fw (or width (frame-width)))
         (cnt (length pairs))
         (best-col-w nil)
         (best-rows nil))
    (cl-loop for col from 5 downto 2  do
             (let* ((row (1+ (/ cnt col)))
                    (v-parts (seq-partition pairs row))
                    (rows (akn-keypad--transpose-lists v-parts))
                    (col-w (thread-last
                             v-parts
                             (mapcar
                              (lambda (col)
                                (cons (seq-max (or (mapcar (lambda (it) (length (car it))) col) '(0)))
                                      (seq-max (or (mapcar (lambda (it) (length (cdr it))) col) '(0))))))))
                    ;; col-w looks like:
                    ;; ((3 . 2) (4 . 3))
                    (w (thread-last
                         col-w
                         ;; 4 is for the width of arrow(3) between key and command
                         ;; and the end tab or newline(1)
                         (mapcar (lambda (it) (+ (car it) (cdr it) 4)))
                         (akn-keypad--sum))))
               (when (<= w fw)
                 (setq best-col-w col-w
                       best-rows rows)
                 (cl-return nil))))
    (if best-rows
        (thread-last
          best-rows
          (mapcar
           (lambda (row)
             (thread-last
               row
               (seq-map-indexed
                (lambda (it idx)
                  (let* ((key-str (car it))
                         (def-str (cdr it))
                         (l-r (nth idx best-col-w))
                         (l (car l-r))
                         (r (cdr l-r))
                         (key (akn-keypad--string-pad key-str l 32 t))
                         (def (akn-keypad--string-pad def-str r 32)))
                    (format "%s%s%s"
                            key
                            (propertize " → " 'face 'font-lock-comment-face)
                            def))))
               (akn-keypad--string-join " "))))
          (akn-keypad--string-join "\n"))
      (propertize "Frame is too narrow for KEYPAD popup" 'face 'akn-keypad-cannot-display))))



(defun akn-keypad-describe-keymap (keymap)
  (when (and keymap (not defining-kbd-macro) (not akn-keypad--help))
    (let* ((rst))
      (map-keymap
       (lambda (key def)
         (let ((k (if (consp key)
                      (format "%s .. %s"
                              (key-description (list (car key)))
                              (key-description (list (cdr key))))
                    (key-description (list key)))))
           (let (key-str def-str)
             (cond
              ((and (commandp def) (symbolp def))
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (symbol-name def) 'face 'font-lock-function-name-face)))
              ((symbolp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (concat "+" (symbol-name def)) 'face 'font-lock-keyword-face)))
              ((functionp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "?closure" 'face 'font-lock-function-name-face)))
              (t
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "+prefix" 'face 'font-lock-keyword-face))))
             (push (cons key-str def-str) rst))))
       keymap)
      (setq rst (reverse rst))
      (let ((msg (akn-keypad--describe-keymap-format rst)))
        (let ((message-log-max)
              (max-mini-window-height 1.0))
          (save-window-excursion
            (with-temp-message
                (format "%s\nKEYPAD: %s%s"
                        msg
                        (let ((pre (akn-keypad--format-prefix)))
                          (if (string-blank-p pre)
                              ""
                            (propertize pre 'face 'font-lock-comment-face)))
                        (propertize (akn-keypad--format-keys nil) 'face 'font-lock-string-face))
              (sit-for 1000000 t))))))))

(defun akn-keypad-get-title (def)
  "Return a symbol as title or DEF.

Returning DEF will result in a generated title."
  (if-let ((cmd (and (symbolp def)
                     (commandp def)
                     (get def 'akn-keypad-dispatch))))
      (akn-keypad--lookup-key (kbd cmd))
    def))

(defun akn-keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond
   (akn-keypad--use-both
    (setq akn-keypad--use-both nil))
   (akn-keypad--use-literal
    (setq akn-keypad--use-literal nil))
   (akn-keypad--use-meta
    (setq akn-keypad--use-meta nil))
   (t
    (pop akn-keypad--keys)))
  (if akn-keypad--keys
      (progn
        ;; (akn-keypad--update-indicator)
        (akn-keypad--display-message))
    (when akn-keypad-message
      (message "KEYPAD exit"))
    (akn-keypad-mode -1)))

(defun akn-keypad--show-message ()
  (let ((message-log-max))
    (message "KEYPAD%s: %s%s"
             (if akn-keypad--help " describe key" "")
             (let ((pre (akn-keypad--format-prefix)))
               (if (string-blank-p pre)
                   ""
                 (propertize pre 'face 'font-lock-comment-face)))
             (propertize (akn-keypad--format-keys nil) 'face 'font-lock-string-face))))

(defun akn-keypad--try-execute ()
  "Try execute command.

If there is a command available on the current key binding,
try replacing the last modifier and try again."
  ;; (message "maybe trying execute")
  (unless (or akn-keypad--use-literal
              akn-keypad--use-meta
              akn-keypad--use-both)
    ;; (message "trying execute")
    (let* ((key-str (akn-keypad--format-keys nil))
           (cmd (akn-keypad--lookup-key (read-kbd-macro key-str))))
      (cond
       ((commandp cmd t)
        (setq current-prefix-arg akn-keypad--prefix-arg
              akn-keypad--prefix-arg nil)
        (if akn-keypad--help
            (progn
              ;; (message "describing")
              (akn-keypad-mode -1)
              (describe-function cmd))
          (let ((akn-keypad--this-command cmd))
            (akn-keypad-mode -1)
            (setq real-this-command cmd
                  this-command cmd)
            ;; (message "running command")
            (call-interactively cmd))))
       ((keymapp cmd)
        (when akn-keypad-message (akn-keypad--show-message))
        (akn-keypad--display-message))
        ;; (message "keymap"))
       ((equal 'control (caar akn-keypad--keys))
        (setcar akn-keypad--keys (cons 'literal (cdar akn-keypad--keys)))
        (akn-keypad--try-execute))
        ;; (message "control"))
       (t
        (setq akn-keypad--prefix-arg nil)
        (message "%s is undefined" (akn-keypad--format-keys nil))
        ;; (message "undefined")
        (akn-keypad-mode -1))))))

(defun akn-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  ;; (message "self insert")
  (setq this-command last-command)
  (when-let ((e (akn-keypad--event-key last-input-event))
             (key (akn-keypad--parse-input-event e)))
    ;; (message "body")
    (let ((has-sub-meta (akn-keypad--has-sub-meta-keymap-p)))
      (cond
       (akn-keypad--use-literal
        (push (cons 'literal key)
              akn-keypad--keys)
        (setq akn-keypad--use-literal nil))
       (akn-keypad--use-both
        (push (cons 'both key) akn-keypad--keys)
        (setq akn-keypad--use-both nil))
       (akn-keypad--use-meta
        (push (cons 'meta key) akn-keypad--keys)
        (setq akn-keypad--use-meta nil))
       ((and (equal e akn-keypad-meta-prefix)
             (not akn-keypad--use-meta)
             has-sub-meta)
        (setq akn-keypad--use-meta t))
       ((and (equal e akn-keypad-ctrl-meta-prefix)
             (not akn-keypad--use-both)
             has-sub-meta)
        (setq akn-keypad--use-both t))
       ((and (equal e akn-keypad-literal-prefix)
             (not akn-keypad--use-literal)
             akn-keypad--keys)
        (setq akn-keypad--use-literal t))
       (akn-keypad--keys
        (push (cons 'control key) akn-keypad--keys))
       ((alist-get e akn-keypad-start-keys)
        (push (cons 'control (akn-keypad--parse-input-event
                              (alist-get e akn-keypad-start-keys)))
              akn-keypad--keys))
       (akn-keypad--allow-quick-dispatch
        (if-let ((keymap (akn-keypad--get-leader-keymap)))
            (setq akn-keypad--base-keymap keymap)
          (setq akn-keypad--keys (akn-keypad--parse-string-to-keypad-keys akn-keypad-leader-dispatch)))
        (push (cons 'literal key) akn-keypad--keys))
       (t
        (push (cons 'control key) akn-keypad--keys))))

    ;; Try execute if the input is valid.
    (if (or akn-keypad--use-literal
            akn-keypad--use-meta
            akn-keypad--use-both)
        (progn
          ;; (message "displaying message")
          (when akn-keypad-message (akn-keypad--show-message))
          (akn-keypad--display-message))
      (akn-keypad--try-execute))))

;;;###autoload
(defun akn-keypad ()
  "Enter keypad state."
  (interactive)
  (setq this-command last-command)
  ;; (setq akn-keypad--previous-state (akn-keypad--current-state))
  (akn-keypad-mode)
  (akn-keypad--display-message))

;;;###autoload
(defun akn-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (setq this-command last-command)
  ;; (setq akn-keypad--previous-state (akn-keypad--current-state))
  (akn-keypad-mode)
  (setq akn-keypad--allow-quick-dispatch nil)
  (call-interactively #'akn-keypad-self-insert))

(defun akn-keypad-start-with (input)
  "Enter keypad state with INPUT.

INPUT is a string, stands for initial keys."
  ;; (setq akn-keypad--previous-state (akn-keypad--current-state))
  (akn-keypad-mode)
  (setq akn-keypad--keys (akn-keypad--parse-string-to-keypad-keys input))
  (akn-keypad--try-execute))

(defun akn-keypad-describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq this-command last-command)
  (setq akn-keypad--help t)
        ;; akn-keypad--previous-state (akn-keypad--current-state))
  (akn-keypad-mode)
  (akn-keypad--show-message)
  (akn-keypad--display-message))

(provide 'akn-keypad)
;;; akn-keypad.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
