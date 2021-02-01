;;; input/csi-u/config.el -*- lexical-binding: t; -*-

;;; CSI u mode for iTerm
;; https://iterm2.com/documentation-csiu.html
;; https://iterm2.com/faq.html
;; https://gist.github.com/gnachman/b4fb1e643e7e82a546bc9f86f30360e4

(defun akn/iterm-p (&optional frame)
  (getenv "ITERM_SESSION_ID" (or frame (selected-frame))))

(add-hook 'after-make-frame-functions #'akn/iterm-csi-u t)
(defun akn/iterm-csi-u (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (and (not (display-graphic-p))
               (akn/iterm-p))
      ;; Take advantage of iterm2's CSI u support (https://gitlab.com/gnachman/iterm2/-/issues/8382).
      (xterm--init-modify-other-keys)

      (let ((modifiers-to-map '(;; with ?.VT100.formatOtherKeys: 0
                                ("\e\[27;3;%d~" meta)
                                ("\e\[27;5;%d~" control)
                                ("\e\[27;6;%d~" control shift)
                                ("\e\[27;7;%d~" control meta)
                                ("\e\[27;8;%d~" control meta shift)
                                ;; with ?.VT100.formatOtherKeys: 1
                                ("\e\[%d;3u" meta)
                                ("\e\[%d;5u" control)
                                ("\e\[%d;6u" control shift)
                                ("\e\[%d;7u" control meta)
                                ("\e\[%d;8u" control meta shift))))
        (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
          (let ((c 32))
            (while (<= c 126)
              (dolist (x modifiers-to-map)
                (define-key xterm-function-map (format (car x) c)
                            (apply #'+csi-u--character-apply-modifiers c (cdr x))))
              (setq c (1+ c))))
          t)))))

;; Courtesy https://emacs.stackexchange.com/a/13957, modified per
;; https://gitlab.com/gnachman/iterm2/-/issues/8382#note_365264207
(defun +csi-u--character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  ;; (when (memq 'control modifiers)
  ;;   (when (<= ?A c ?Z)
  ;;     (setq modifiers (cons 'shift modifiers))
  ;;     (setq c (downcase c)))
  ;;   (setq c (if (<= ?a c ?z)
  ;;               (logand c ?\x1f)
  ;;             (logior (ash 1 26) c))))
  ;; (when (memq 'meta modifiers) (setq c (logior (ash 1 27) c)))
  ;; (when (memq 'shift modifiers) (setq c (logior (ash 1 25) c)))
  (when (memq 'control modifiers)
    (when (<= ?A c ?Z)
      (setq modifiers (cons 'shift modifiers)))
    (setq c (if (<= 64 (upcase c) 95)
                (- (upcase c) 64)
              (logior (ash 1 26) c))))
  (dolist (m '(meta shift hyper super alt))
    (when (memq m modifiers)
      (setq c (event-apply-modifier
               c
               m
               (pcase m
                 ('meta 27)
                 ('control 26)
                 ('shift 25)
                 ('hyper 24)
                 ('super 23)
                 ('alt 22)
                 (_ (error "unrecognized modifier: %s" m)))
               (pcase m
                 ('meta "M-")
                 ('control "C-")
                 ('shift "S-")
                 ('hyper "H-")
                 ('super "s-")
                 ('alt "A-")
                 (_ (error "unrecognized modifier: %s" m)))))))
  (vector c))

;; ?-
;; #x000002d
;; ?_
;; #x000005f
;; ?\C--
;; #x400002d
;; ?\C-_
;; #x000001f

;; (event-apply-modifier ?- 'control 26 "C-")
;; (event-apply-modifier ?_ 'control 26 "C-")


;; #x400002d
;; #x000001f
;; #x400005f

;; (map! "C-R" #'ignore)
;; "C-S-r"
;; "C-R"
;; ?\C-r
;; ?\C-R
;; (eq (logior (lsh 1 25) ?\C-r) ?\C-\S-r)
;; (aref (read-kbd-macro "C-S-r" t) 0)
;; (aref (read-kbd-macro "C-R" t) 0)

;; (kbd "C-r")
;; (kbd "C-R")
;; (kbd "C-S-r")

;; (list
;;  (aref (+csi-u--character-apply-modifiers (aref "" 0)) 0)
;;  (aref (+csi-u--character-apply-modifiers (aref "" 0) 'control) 0)
;;  (aref (+csi-u--character-apply-modifiers (aref "" 0) 'control 'shift) 0)
;;  (aref (+csi-u--character-apply-modifiers ?R) 0)
;;  (aref (+csi-u--character-apply-modifiers ?R 'control) 0)
;;  (aref (+csi-u--character-apply-modifiers ?R 'control 'shift) 0)
;;  (aref (+csi-u--character-apply-modifiers ?r) 0)
;;  (aref (+csi-u--character-apply-modifiers ?r 'control) 0)
;;  (aref (+csi-u--character-apply-modifiers ?r 'control 'shift) 0))
;; (aref "" 0)
;; (- ?R 64)
;; 33554450  #x2000012
;; 100663410 #x6000072 ;bad
;; 100663314 #x6000012
;; #x12
;; (list
;;  (logand ?r ?\x1f)
;;  (logand ?R ?\x1f)
;;  (logior (lsh 1 26) ?r)
;;  (logior (lsh 1 26) ?R))

;; (setq translate-upper-case-key-bindings t)
