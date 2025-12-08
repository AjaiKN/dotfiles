;;; akn.el --- My miscellanous elisp utilities library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
;;
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Version: 0.0.1
;; Homepage: https://github.com/AjaiKN/dotfiles
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'map)
(eval-when-compile
  (require 'tramp)
  (require 'macroexp)
  (require 'doom nil t)
  (require 'akn-doom-use-package nil t)
  ;; (require 'doom-packages)
  ;; (require 'doom-modules)
  (require 'doom-ui nil t))
(eval-and-compile
  (require 'doom-lib nil t)
  (require 'compat)
  (require 'help-fns))

(defgroup akn nil
  "Ajai's stuff."
  :group 'emacs)

;;; boundp/fboundp helpers
(defmacro akn/boundp (&rest args)
  (cons 'and
        (cl-loop for arg in args
                 collect
                 (pcase arg
                   (`(function ,fn) `(fboundp ',fn))
                   ((pred symbolp) `(boundp ',arg))
                   (_ (error "akn/boundp: not a symbol or sharp-quoted function: %s" arg))))))

(defmacro akn/fboundp-and (form)
  `(and (fboundp ',(car form))
        ,form))

(defmacro akn/fboundp*-and (form)
  (let ((forms (list form))
        (checks))
    (while-let ((f (pop forms)))
      (pcase f
        ((pred symbolp) nil)
        (`(function ,fn) (push `(fboundp ',fn) checks))
        (`(,(and (pred symbolp) fn) . ,rest)
         (push `(fboundp ',fn) checks)
         (setq forms (append forms rest)))
        (`(,(pred listp) . ,_)
         (setq forms (append forms f)))))
    (append (list 'and)
            (nreverse checks)
            (list form))))
;; (akn/fboundp*-and ((abc r) (q r 3 (x r) #'z)))

(defmacro akn/boundp*-and (form)
  (let ((forms (list form))
        (checks))
    (while-let ((f (pop forms)))
      (pcase f
        ((pred symbolp)  (push `(boundp ',f) checks))
        (`(function ,fn) (push `(fboundp ',fn) checks))
        (`(,(and (pred symbolp) fn) . ,rest)
         (push `(fboundp ',fn) checks)
         (setq forms (append forms rest)))
        (`(,(pred listp) . ,_)
         (setq forms (append forms f)))))
    (append (list 'and)
            (nreverse checks)
            (list form))))
;; (akn/boundp*-and ((abc r) (q r 3 (x r) #'z)))

;;; symbols and keywords
(eval-and-compile
  (defalias 'akn/symbol->string #'symbol-name)
  (defalias 'akn/string->symbol #'intern)

  (defmacro akn/symbol-format (format-str &rest args)
    `(akn/string->symbol (format ,format-str ,@args)))

  (defun akn/symbol->keyword (symbol)
    (akn/symbol-format ":%s" symbol))
  (defun akn/keyword->symbol (keyword &optional noerror)
    (if (keywordp keyword)
        (akn/-> keyword
                (akn/symbol->string)
                (substring 1)
                (akn/string->symbol))
      (unless noerror
        (error "akn/keyword->symbol: %s is not a keyword" keyword))))

  (defun akn/unquote (exp)
    "Return EXP unquoted.

Same as Doom's `doom-unquote'."
    (declare (pure t) (side-effect-free t))
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp))

(defmacro akn/keywordify (my-name orig-fn arg-names)
  (declare (indent defun))
  (setq orig-fn (akn/unquote orig-fn))
  (setq arg-names (mapcar #'akn/symbol->keyword arg-names))
  `(defmacro ,my-name (&rest inner-args)
     (cl-destructuring-bind
         (position-args keyword-args)
         (cl-loop for rest-args on inner-args
                  for arg = (car rest-args)
                  until (keywordp arg)
                  collect arg into position-args
                  finally return (list position-args rest-args))
       (cl-loop for keyword in keyword-args by #'cddr
                do (when (not (memq keyword ',arg-names))
                     (warn "%s is not a valid argument to %s" keyword ',my-name)))
       (let ((inner-arg-names (drop (length position-args) ',arg-names)))
         (append ',(if (symbolp orig-fn) (list orig-fn) (list 'funcall orig-fn))
                 position-args
                 (cl-loop for name in inner-arg-names
                          collect (plist-get keyword-args name)))))))

(akn/keywordify akn/read-file-name #'read-file-name
  (prompt dir default-filename mustmatch initial predicate))

;;; non-doom compat

(defmacro akn--simple-modulep! (group module)
  `(and (get ,group ',module) t))

(defmacro akn/log (&rest args)
  `(when (or debug-on-error (bound-and-true-p doom-debug-mode))
     (message ,@args)))

(defmacro akn/evil-save-state (&rest stuff)
  `(if (and (bound-and-true-p evil-local-mode) (fboundp 'evil-save-state))
       (evil-save-state ,@stuff)
     (progn ,@stuff)))

;;; akn/load!

(defmacro akn/load! (file &optional path noerror)
  (when (string-match (rx bos (group (+ any)) ".el" eos) file)
    (setq file (match-string-no-properties 1 file)))
  `(condition-case-unless-debug err
       (load! ,file ,path ,noerror)
     (error
      (display-warning 'akn/load!
                       (format-message "error when loading `%s.el': %S" ,file err)
                       :error)
      nil)))

;;; Rotating text
(cl-defun akn/add-rotate-patterns! (hooks &key symbols words patterns)
  "Declare :symbols, :words or :patterns (all lists of strings) that
`rotate-text' will cycle through.

This is just like Doom's `set-rotate-patterns!', except it adds to
the variables instead of replacing them."
  (declare (indent defun))
  (when (akn--simple-modulep! :editor rotate-text)
    (dolist (hook (ensure-list hooks))
      (let ((fn-name (gensym (format "akn/+rotate-text-init-h/%s" hook))))
        (fset fn-name
              (lambda ()
                (setq-local rotate-text-local-symbols (append symbols (bound-and-true-p rotate-text-local-symbols)))
                (setq-local rotate-text-local-words (append words (bound-and-true-p rotate-text-words)))
                (setq-local rotate-text-local-patterns (append patterns (bound-and-true-p rotate-text-patterns)))))
        ;; make it depth 1 so that it runs after Doom's +rotate-text-init-%s-h hook
        (add-hook hook fn-name 1)))))

(defun akn/rotate-symbols! (hooks &rest symbols)
  (when (akn--simple-modulep! :editor rotate-text)
    (akn/add-rotate-patterns! hooks
      :symbols (cond ((and (not (cdr symbols))
                           (stringp (car-safe (car symbols))))
                      (car symbols))
                     ((listp (car symbols))
                      symbols)
                     ((stringp (car symbols))
                      (list symbols))
                     (t (warn "akn/rotate-symbols! bad input") nil)))))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "setq-hook!" "unsetq-hook!")

;;; some convenient-to-rotate functions/macros
(defmacro akn/remove-from-list (list-var element &optional _append compare-fn)
  ""
  (setq list-var (akn/unquote list-var))
  (if compare-fn
      `(cl-callf2 cl-delete ,element ,list-var :test ,compare-fn)
    `(cl-callf2 delete ,element ,list-var)))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "add-to-list" "akn/remove-from-list")

(defmacro akn/removeall! (place &rest values)
  ""
  (cl-with-gensyms (x vals)
    `(let ((,vals (list ,@values)))
       (cl-callf2
           cl-delete-if
           (lambda (,x) (member ,x ,vals))
           ,place))))

(akn/rotate-symbols! 'emacs-lisp-mode-hook
                    '("pushnew!" "akn/removeall!")
                    '("cl-pushnew" "akn/removeall!"))

(defmacro akn/remove-hook (hook function &optional _depth local)
  "Like `remove-hook', but takes the same arguments as `add-hook' for convenience."
  (if local
      `(remove-hook ,hook ,function ,local)
    `(remove-hook ,hook ,function)))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "add-hook" "akn/remove-hook")

(cl-defmacro akn/undefine-advice (symbol (_how lambda-list &optional name _depth) &rest body)
  (let ((advice-fn-name (and name (akn/string->symbol (format "%s@%s" symbol name)))))
    `(progn
       (advice-remove #',symbol
                      #',(or advice-fn-name `(lambda ,lambda-list ,@body)))
       ,(when advice-fn-name
          `(fmakunbound ',advice-fn-name)))))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "define-advice" "akn/undefine-advice")

;;; convenience macros for setting variables then restoring the old values
(defmacro akn/mode-set (my-mode-name &rest vars-and-values)
  "To be used inside a define-minor-mode.

Sets the variables to the specified values when the mode is
enabled, and when disabled resets them to their original values."
  (declare (indent 1))
  (let ((restore-var (akn/symbol-format "%s--akn/restore" my-mode-name)))
    `(progn
       (defvar-local ,restore-var nil)
       (if ,my-mode-name
           (setq ,restore-var (buffer-local-set-state ,@vars-and-values))
         (buffer-local-restore-state ,restore-var)))))
(defmacro akn/mode-add-hook! (my-mode-name &rest body)
  "To be used in a mode definition."
  (declare (indent defun)
           (debug t))
  `(if ,my-mode-name
       (add-hook! ,@body)
     (remove-hook! ,@body)))

;;; functions for files
(defun akn/existing-file (f)
  (and f
       (file-exists-p f)
       f))
(defun akn/existing-file-truename (f)
  (and f
       (file-exists-p f)
       (file-truename f)))
(defun akn/expand-file (f)
  (or (and f (file-truename f))
      f))
(defun akn/expand-file-downcase (f)
  (setq f (or (and f (file-truename f))
              f))
  (and f (downcase f)))

;;;###autoload
(defun akn/system-find-file ()
  "https://christiantietze.de/posts/2022/12/use-file-open-dialog-for-file-actions/"
  (interactive)
  (let ((last-nonmenu-event nil)
        (use-dialog-box t)
        (use-file-dialog t))
    (call-interactively #'find-file)))

;;; aliases
(eval-and-compile
  (defalias 'akn/-> #'thread-first)
  (defalias 'akn/->> #'thread-last))

(defalias 'akn/unfontify #'substring-no-properties)

(defalias 'akn/buffer-visible-p #'get-buffer-window)

(defalias 'akn/doom/bump-packages-in-module #'doom/bump-module)

;;;###autoload
(defvaralias 'akn/format-on-save-mode 'apheleia-mode)
;;;###autoload
(defalias 'akn/format-on-save-mode (if (fboundp 'apheleia-mode)
                                       #'apheleia-mode
                                     #'ignore))

;;; macros for defining things
(defmacro akn/defun (name &rest stuff)
  "Just like `defun', except return NAME.

(`defun' seems to do this too, but the documentation says `defun''s
return value is undefined.)

STUFF is just the rest of the arguments passed to `defun'."
  (declare (indent defun))
  `(progn (defun ,name ,@stuff)
          ',name))

(defmacro akn/defvar-setq (symbol &optional initvalue docstring)
  (declare (doc-string 3) (debug (name body))
           (indent defun))
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq ,symbol ,initvalue)))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "defvar" "akn/defvar-setq")

;;; akn/letf!

(defmacro akn/letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Based on Doom's `letf!'.

Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice (`define-advice').

BINDINGS is either:

  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.
    PLACE can also be a sharp-quoted function, which will be converted to a
    `symbol-function' call that's accepted by `cl-letf*'.
    If PLACE is just a regular symbol, it will always be dynamically (rather
    than lexically) bound.
  A list of, or a single, def* forms.

The def* forms accepted are:

  (defun NAME (ARGS...) &rest BODY)
    Defines a temporary function with `cl-letf'
  (defun* NAME (ARGS...) &rest BODY)
    Defines a temporary function with `cl-labels' (allows recursive
    definitions).
  (defmacro NAME (ARGS...) &rest BODY)
    Uses `cl-macrolet'.
  (defadvice! SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY)
    Uses Doom's `defadvice!' (then `undefadvice!' afterwards).
  (advice-add FUNCTION WHERE ADVICE)
    Uses `advice-add' (then `advice-remove' afterwards).
  (define-advice FUNCTION (HOW LAMBDA-LIST &optional NAME DEPTH) &rest BODY)
    Defines temporary advice with `define-advice'.
  (add-hook HOOK FUNCTION &optional DEPTH LOCAL)
    Adds a hook temporarily using `add-hook' (then `remove-hook' afterwards).
  (add-hook! HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)
    Adds a hook temporarily using Doom's `add-hook!' (then `remove-hook!'
    afterwards).
  (defvar SYMBOL &optional INITVALUE)
    Declares a temporary, dynamically-scoped variable, and if it's not already
    bound, sets it temporarily to INITVALUE.
  (defvar* SYMBOL VALUE)
  (defconst SYMBOL VALUE)
  (akn/defvar-setq SYMBOL VALUE)
    Declares a temporary, dynamically-scoped variable, and sets it temporarily
    to VALUE (even if it's already set). This is the same as just using
    a (SYMBOL VALUE) form."
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings)
              '(advice-add
                defadvice! define-advice defadvice
                defmacro
                defun defun*
                akn/advise-letf!
                add-hook add-hook!
                defvar defvar* defconst akn/defvar-setq
                ;; not (yet?) implemented
                set setf setq setq! setcar setcdr setenv setopt setplist
                setq-hook! setq-local set-default set-register set-variable
                setq-default setq-mode-local set-default-toplevel-value
                defalias defclass defsubst defvar-1 defcustom define-inline
                define-keymap defvar-keymap))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro
               `(cl-macrolet ((,@rest)) ,@(macroexp-unprogn body)))
              (`defadvice!
               `(unwind-protect
                    (progn (defadvice! ,@rest)
                           ,@(macroexp-unprogn body))
                  (undefadvice! ,@rest)))
              (`advice-add
               (cl-destructuring-bind (target where fn &optional props) rest
                 (macroexp-let2* (target fn)
                   `(unwind-protect
                        (progn (advice-add ,target ,where ,fn ,props)
                               ,@(macroexp-unprogn body))
                      (advice-remove ,target ,(or (alist-get 'name props) fn))))))
              ;; defadvice is for compatibility with doom
              ((or `define-advice `defadvice)
               (when (eq type `defadvice)
                 (warn "akn/letf!: please use defadvice!, advice-add, or define-advice instead of defadvice"))
               `(unwind-protect
                    (progn (define-advice ,@rest)
                           ,@(macroexp-unprogn body))
                  (akn/undefine-advice ,@rest)))
              (`defun
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  (cl-letf (((symbol-function #',(car rest))
                             (cl-function (lambda ,(cadr rest) ,@(cddr rest)))))
                    ,@(macroexp-unprogn body))))
              (`defun*
               `(cl-labels ((,@rest))
                  ,@(macroexp-unprogn body)))
              (`akn/advise-letf!
               `(unwind-protect
                    (progn (akn/advise-letf! ,@rest)
                           ,@(macroexp-unprogn body))
                  (akn/unadvise-letf! ,@rest)))
              (`add-hook
               `(unwind-protect
                    (progn (add-hook ,@rest)
                           ,@(macroexp-unprogn body))
                  (akn/remove-hook ,@rest)))
              (`add-hook!
               `(unwind-protect (progn (add-hook! ,@rest)
                                       ,@(macroexp-unprogn body))
                  (remove-hook! ,@rest)))
              (`defvar
               (cl-destructuring-bind (symbol &optional initvalue _docstring) rest
                 `(let (_)
                    (defvar ,symbol)
                    ,@(if (length> rest 1)
                          `((if (boundp ',symbol)
                                ,body
                              (let ((,symbol ,initvalue))
                                ,@(macroexp-unprogn body))))
                        (macroexp-unprogn body)))))
              ((or `defvar* `defconst `akn/defvar-setq)
               (cl-destructuring-bind (symbol initvalue &optional _docstring) rest
                 `(dlet ((,symbol ,initvalue))
                    ,@(macroexp-unprogn body))))
              ((pred symbolp)
               (cl-destructuring-bind (symbol initvalue) binding
                 `(dlet ((,symbol ,initvalue))
                    ,@(macroexp-unprogn body))))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               `(cl-letf ((,type ,@rest))
                  ,@(macroexp-unprogn body))))))))

;;; akn/completing-read

;; also see https://kisaragi-hiu.com/emacs-completion-metadata/

(cl-defun akn/completing-read (prompt table
                                      &rest options
                                      &key predicate require-match history default
                                      initial inherit-input-method
                                      keymap category narrow add-history annotate state preview-key sort lookup group
                                      command initial-narrow async-wrap)
  "Calls `completing-read', or `consult--read' if available."
  (ignore keymap category narrow add-history annotate state preview-key sort lookup group
          command initial-narrow async-wrap) ;avoid unused variable warnings
  (if (require 'consult nil 'noerror)
      (apply 'consult--read table :prompt prompt options)
    (completing-read prompt table predicate require-match initial history default inherit-input-method)))

;; https://www.howardism.org/Technical/Emacs/alt-completing-read.html
(cl-defun akn/completing-read+ (prompt collection
                                       &rest args
                                       &key default &allow-other-keys)
  "Calls `completing-read' but returns the value from COLLECTION.

Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice. For instance,
give a variable of choices like:

    (defvar favorite-hosts \\='((\"Glamdring\" . \"192.168.5.12\")
                             (\"Orcrist\"   . \"192.168.5.10\")
                             (\"Sting\"     . \"192.168.5.220\")
                             (\"Gungnir\"   . \"192.168.5.25\")))

We can use this function to `interactive' without needing to call
`alist-get' afterwards:

    (defun favorite-ssh (hostname)
      \"Start a SSH session to a given HOSTNAME.\"
      (interactive
       (list
        (akn/completing-read+ \"Host: \" favorite-hosts)))
      (message \"Rockin' and rollin' to %s\" hostname))"
  (let* ((choice
          (apply #'akn/completing-read prompt collection args))
         (results (cond
                   ((hash-table-p collection)     (gethash choice collection))
                   ((consp (car-safe collection)) (alist-get choice collection default nil #'equal))
                   (t                             choice))))
    (if (listp results) (car results) results)))

;;; macros for advising

(cl-defmacro akn/advise-letf! (wrapped-fun (&optional wrapper-name) &rest stuff)
  (declare (indent 2))
  (cl-with-gensyms (oldfun args)
    `(define-advice ,wrapped-fun (:around (,oldfun &rest ,args) ,wrapper-name)
       (akn/letf! ,stuff
         (apply ,oldfun ,args)))))
(cl-defmacro akn/unadvise-letf! (wrapped-fun (&optional wrapper-name) &rest stuff)
  (declare (indent 2))
  (cl-with-gensyms (oldfun args)
    `(akn/undefine-advice ,wrapped-fun (:around (,oldfun &rest ,args) ,wrapper-name)
       (akn/letf! ,stuff
         (apply ,oldfun ,args)))))
(akn/rotate-symbols! 'emacs-lisp-hook "akn/advise-letf!" "akn/unadvise-letf!")

(defmacro akn/make-overrider! (func &optional default-func)
  (setq func (akn/unquote func))
  (let ((overrider (akn/symbol-format "akn/overrider/%s-a" func))
        (var-name  (akn/symbol-format "akn/overrider/%s-function" func))
        (original  (akn/symbol-format "akn/overrider/%s-original-function" func)))
    `(progn
       (defcustom ,var-name ,default-func
         ,(format "The function that's actually called when you call `%1$s'.

`%1$s' has been advised to call
the function stored in this
variable instead of calling the
original `%1$s' function. If this
variable is nil, the original `%1$s'
will be called.

You can also use `%2$s'
to call the original function." func original)
         :group 'akn
         :type '(choice
                 (function :tag "Overriding function")
                 (const nil :tag "Do not override"))
         :risky t)
       (defun ,original (&rest args)
         ,(format "Call the original `%s' function
without overriding it.

This function temporarily sets
`%s' to nil so that the
effects of `akn/make-overrider!' don't apply." func var-name)
         (let ((,var-name nil))
           (apply #',func args)))
       (defconst ,original #',original
         ,(format "The name of function that calls the original definition of `%s'
without overriding it." func))
       (defadvice! ,overrider (fn &rest args)
         ,(format "Advice that calls the function stored in
`%1$s' instead of the original function.

If `%1$s' is nil,
this advice does nothing.

You can also use `%3$s'
to call the original `%2$s'
function." var-name func original)
         :around #',func
         (apply (or ,var-name fn) args)))))

(defun akn/function-advices (sym)
  (and (fboundp sym)
       (let ((advices nil))
         (advice-mapc (lambda (advice prop-alist) (push (cons advice prop-alist) advices))
                      sym)
         advices)))
(defalias 'akn/advised-p #'akn/function-advices)

(defun akn/function-advices-with-how (symbol)
  "Returns an alist from advices on SYMBOL to their HOWs.

The `car' of each element is the advice function, and the `cdr'
is :around, :before, :after, :override, :after-until,
:after-while, :before-until, :before-while, :filter-args, or
:filter-return."
  ;; from advice--make-docstring
  (cl-loop with flist = (indirect-function symbol)
           while (advice--p flist)
           collect (cons (advice--car flist) (advice--how flist))
           do (setq flist (advice--cdr flist))))

(defun akn/advice-remove (symbol _how function &optional props)
  "Like `advice-remove', but takes the same arguments as
`advice-add' for convenience, and is interactive."
  (interactive
   (let* ((symbol
           (intern
            (akn/completing-read "Function to remove advice from: "
                                 obarray
                                 :predicate #'akn/advised-p)))
          (advices (akn/function-advices-with-how symbol))
          (function
           (intern
            (akn/completing-read "Advice to remove: "
                                 advices
                                 :category 'function
                                 :annotate (lambda (candidate)
                                             (list candidate
                                                   ;; prefix
                                                   (propertize
                                                    (let ((advice (intern candidate)))
                                                      (format "%-16s" (alist-get advice advices "" nil #'equal)))
                                                    'face 'completions-annotations)
                                                   ;; postfix
                                                   nil))))))
     (list symbol nil function)))
  (advice-remove symbol (or (alist-get 'name props) function)))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "advice-add" "akn/advice-remove")

;;; advices

;;;###autoload
(defun akn/ignore-errors-a (fn &rest args)
  (ignore-errors (apply fn args)))
;;;###autoload
(defun akn/demote-errors-a (fn &rest args)
  (with-demoted-errors "demoted error: %S" (apply fn args)))

;;;###autoload
(defun akn/save-selected-window-a (fn &rest args)
  (save-selected-window
    (apply fn args)))

;;;###autoload
(defun akn/inhibit-read-only-a (fn &rest args)
  (let ((inhibit-read-only t))
    (apply fn args)))

;;;###autoload
(defun akn/always-steal-lock-a (fn &rest args)
  (akn/letf! ((#'ask-user-about-lock #'always))
    (apply fn args)))

;;;###autoload
(defun akn/always-ignore-lock-a (fn &rest args)
  (akn/letf! ((#'ask-user-about-lock #'ignore))
    (apply fn args)))

;;; function for prioritizing keymaps
(defun akn/prioritize-minor-mode-keymap (minor-mode-name)
  (when (not (eq (caar minor-mode-map-alist) minor-mode-name))
    (let ((entry (assq minor-mode-name minor-mode-map-alist)))
      (if (not entry)
          (warn "there's no entry for minor mode %s in minor-mode-map-alist" minor-mode-name)
        (setq minor-mode-map-alist (assq-delete-all minor-mode-name minor-mode-map-alist))
        (push entry minor-mode-map-alist)))))

;;; akn/after-hook!
(defvar akn/hooks-to-record nil
  "See `akn/after-hook!'")
(setq akn/hooks-to-record '(doom-first-file-hook
                            doom-first-input-hook
                            doom-first-buffer-hook
                            doom-init-ui-hook
                            akn/evil-respect-line-move-visual-mode-hook
                            akn/terminal-quit-mode-hook
                            doom-after-init-hook))
(defun akn/will-record-hook-p (hook) (memq hook akn/hooks-to-record))
(defvar akn/hooks-already-run '())
(defun akn/has-run-hook-p (hook) (memq hook akn/hooks-already-run))
(dolist (hook akn/hooks-to-record)
  (add-hook hook (lambda (&rest _) (push hook akn/hooks-already-run))))
(defun akn/after-hook! (hooks fn)
  "If any of the HOOKS have already been run, run FN now.
Otherwise, add FN to the HOOKS.

For this to work, the hook must be part of `akn/hooks-to-record'.

This is so that when I add a hook on `doom-first-file-hook' (or similar)"
  (declare (indent 1))
  (setq hooks (ensure-list hooks))
  (if (seq-some #'akn/has-run-hook-p hooks)
      (funcall fn)
    (when (not (seq-every-p #'akn/will-record-hook-p hooks))
      (debug)
      (setq akn/hooks-to-record (seq-uniq (append akn/hooks-to-record hooks)))
      (warn "All hooks in %s should be added to the initial value of akn/hooks-to-record -> %s"
            hooks
            akn/hooks-to-record))
    (dolist (h hooks) (add-hook h fn))))

;;; helpers for macros
(eval-and-compile
  (defun akn/function-body->function+args (fbody &optional prohibit-args)
    (cond
     ((null fbody) (list `#'ignore))
     ((and (null (cdr fbody)) ;length < 1
           (if prohibit-args
               (null (cdar fbody))
             (seq-every-p #'macroexp-const-p (cdar fbody))))
      (cons (if (symbolp (caar fbody))
                `#',(caar fbody)
              (caar fbody))
            (cdar fbody)))
     (t
      (list `(lambda () ,@fbody)))))

  (defun akn/function-body->function (fbody)
    (car (akn/function-body->function+args fbody t)))

  (defun akn/macro/setq-if (var-name body)
    (if var-name
        `(setq ,var-name ,body)
      body)))

;;; akn/after-timer!
(cl-defmacro akn/after-timer! ((seconds &key repeat) &rest body)
  "Run BODY after Emacs has been idle for SECONDS seconds.

If REPEAT is non-nil, run BODY every time Emacs has been idle for REPEAT
seconds (or SECONDS seconds, if REPEAT isn't a number)."
  (declare (indent 1))
  (macroexp-let2* (seconds
                   (repeat (cond
                            ((numberp repeat) repeat)
                            ((eq repeat nil) nil)
                            ((eq repeat t) seconds)
                            (`(and ,repeat (if (numberp ,repeat) ,repeat ,seconds))))))
    `(run-with-timer
      ,seconds
      ,repeat
      ,@(akn/function-body->function+args body))))

;;; akn/after-idle!
(cl-defmacro akn/after-idle! ((seconds &key
                                      each-idle
                                      repeat
                                      starting-now
                                      (timer-name (gensym "akn/timer"))
                                      (cancel-old-timer t))
                              &rest body)
  "Run BODY after Emacs has been idle for SECONDS seconds.

If EACH-IDLE is non-nil, then every time Emacs goes idle for SECONDS,
BODY will be run. But note that BODY will only be run once each time
Emacs is idle; if you leave Emacs idle for 20 minutes all at once, a
2-minute idle timer will only run once.

If you want it to be run 10 times in that example, use REPEAT instead.
It's best to specify the TIMER-NAME when using REPEAT.

If STARTING-NOW is nil, then if, for example, Emacs has already been
idle for 10 seconds when this macro is run, those 10 seconds will
count towards the idle time, so if SECONDS is 20, BODY will be run
in 10 seconds (assuming Emacs stays idle)."
  (declare (indent 1))
  (if repeat
      `(akn/after-idle-repeat! (,seconds :timer-name ,timer-name)
         ,@body)
    ;; TODO: check if starting-now is actually working
    ;; TODO: handle :starting-now t with :each-idle t
    `(progn
       (defvar ,timer-name nil)
       ,(when cancel-old-timer
          `(when (timerp ,timer-name)
             (cancel-timer ,timer-name)))
       (setq ,timer-name
             (run-with-idle-timer
              ,(if starting-now
                   `(time-add (or (and ,starting-now (not ,each-idle) (current-idle-time))
                                  0)
                              ,seconds)
                 seconds)
              ,each-idle
              ,@(akn/function-body->function+args body))))))

(defun akn/timer-activated-p (timer &optional type)
  (or (unless (eq type 'timer) (memq timer timer-idle-list))
      (unless (eq type 'idle)  (memq timer timer-list))))

;;; akn/after-idle-repeat!
;; TODO: add back INITIAL-SECONDS and STARTING-NOW
(cl-defmacro akn/after-idle-repeat! ((seconds &key
                                              (timer-name (gensym "akn/timer")))
                                     &rest body)
  "Run BODY every time Emacs is idle for SECONDS seconds.

Unlike the REPEAT argument of `run-with-idle-timer' (or the EACH-IDLE option
of `akn/after-idle!'), this can run multiple times each time Emacs is idle.
If you leave Emacs idle for 20 minutes all at once, a
2-minute idle timer will be run 10 times."
  (declare (indent 1))
  (cl-with-gensyms (idle-time)
    `(progn
       (defvar ,timer-name nil)
       (when ,timer-name (cancel-timer ,timer-name))
       (setq
        ,timer-name
        (akn/after-timer! (,seconds :repeat t)
          (let ((,idle-time (current-idle-time)))
            (cond
             ((or (null ,idle-time)
                  (< (float-time ,idle-time) ,seconds))
              (timer-set-time ,timer-name
                              (time-add (current-time)
                                        (- ,seconds
                                           (if ,idle-time (float-time ,idle-time) 0)))
                              ,seconds)
              (cancel-timer ,timer-name)
              (timer-activate ,timer-name))
             (t
              ,@body))))))))

;; (akn/after-idle-repeat! (1 :timer-name akn--test-timer)
;;  (message "zhi"))
;; (cancel-timer akn--test-timer)

;;; thunks
(defmacro akn/thunk (&rest body)
  (declare (indent 0))
  `(lambda () ,@body))
(defmacro akn/thunk* (&rest body)
  (declare (indent 0))
  `(lambda (&rest _) ,@body))

;;; akn/with-done-message
(defmacro akn/with-done-message (message &rest body)
  (declare (indent 1))
  (cl-with-gensyms (msg)
    `(let ((,msg (format-message ,message)))
       (prog2
           (message "%s" ,msg)
           ,(macroexp-progn body)
         (message "%sdone" ,msg)))))

;;; incrementally doing stuff to speed up emacs initial load
;; eventually switch to https://github.com/emacs-magus/once ?

(defvar akn/incremental-idle-timer nil)

(defvar akn/incremental-thunks nil)
(cl-defmacro akn/incrementally! ((&key append) &rest body)
  "Add body to the list of functions to run incrementally when Emacs is idle.

This is similar to Doom's :defer-incrementally, except instead of just loading
packages, it can be applied to anything."
  (declare (indent defun))
  `(when akn/incremental-time-rest
     (if (eql akn/incremental-time-rest 0)
         (with-demoted-errors "akn/incremental: error! %S"
           (akn/log "akn/incremental: immediately executing %S" ',(macroexp-progn body))
           ,@body)
       (when (and (not (memq akn/incremental-idle-timer timer-idle-list))
                  (not (memq akn/incremental-idle-timer timer-list)))
         (setq akn/incremental-idle-timer (akn/after-timer! (akn/incremental-time-rest :repeat t)
                                             (akn/incremental-go-rest))))
       ,(if append
            `(cl-callf append akn/incremental-thunks (list (lambda () ,@body)))
          `(push (lambda () ,@body) akn/incremental-thunks)))))
(defvar akn/incremental-time-rest (if (daemonp) 0 0.45))
(defun akn/incremental-go-rest ()
  (let (idle-time)
    (cond
     ((not akn/incremental-time-rest)
      (akn/log "akn/incremental is disabled"))
     ((eql akn/incremental-time-rest 0)
      (while akn/incremental-thunks
        (akn/log "akn/incremental: immediately executing %S" (car akn/incremental-thunks))
        (funcall (pop akn/incremental-thunks))))
     ((not akn/incremental-thunks)
      (akn/log "akn/incremental: done!")
      (cancel-timer akn/incremental-idle-timer))
     ((or (null (setq idle-time (current-idle-time)))
          (< (float-time idle-time) akn/incremental-time-rest))
      (akn/log "akn/incremental: next running in %s (%s  --->  %s)" (- akn/incremental-time-rest (if idle-time (float-time idle-time) 0)) (format-time-string "%F %T.%3N" (current-time)) (format-time-string "%F %T.%3N" (time-add (current-time) (- akn/incremental-time-rest (if idle-time (float-time idle-time) 0)))))
      (timer-set-time akn/incremental-idle-timer
                      (time-add (current-time)
                                (- akn/incremental-time-rest
                                   (if idle-time (float-time idle-time) 0)))
                      akn/incremental-time-rest)
      (cancel-timer akn/incremental-idle-timer)
      (timer-activate akn/incremental-idle-timer))
     (t
      (akn/log "akn/incremental: executing %S" (car akn/incremental-thunks))
      (funcall (pop akn/incremental-thunks))))))

(when akn/incremental-idle-timer (cancel-timer akn/incremental-idle-timer))
(setq akn/incremental-idle-timer (akn/after-timer! (akn/incremental-time-rest :repeat t)
                                  (akn/incremental-go-rest)))
;; (akn/incrementally! (message "hi0"))
;; (akn/incrementally! (message "hi1"))
;; (akn/incrementally! (message "hi2"))
;; (akn/incrementally! (message "hi3"))
;; (akn/after-idle! 1
;;   (message "hi1")
;;   (akn/after-idle! 1
;;     (message "hi2"))
;;   (akn/after-idle! 2
;;     (message "hi3")))

;;; doom incremental packages helpers
(defvar doom-incremental-packages)

(defun akn/doom-prioritize-load-packages-incrementally (packages)
  (cl-callf2 append packages (cdr doom-incremental-packages)))
(akn/rotate-symbols! 'emacs-lisp-mode-hook "doom-load-packages-incrementally" "akn/doom-prioritize-load-packages-incrementally")

;;; remote files / tramp

;; TODO: try `without-remote-files'

(defconst akn/file-remote-regexp (rx bos "/" (+ (not (any "/:"))) ":")
  "Regex for remote files. Same as `ffap-ftp-regexp'.

Also see `tramp-file-name-regexp'.")

(cl-defun akn/file-remote-p (&optional (file default-directory))
  "Approximate equivalent to `file-remote-p', but should be faster."
  (and (stringp file)
       (string-match-p akn/file-remote-regexp file)))

(defalias 'akn/tramp-p #'akn/file-remote-p)

(cl-defun akn/dir-slow-p (&optional (d default-directory))
  "Would this directory likely be slow to access?"
  (and (stringp d)
       (akn/file-remote-p d)
       (not (string-match-p (rx ":" (or "utm" "localhost")) d))))

(defvar akn/tramp-home-dir--truename-cache (make-hash-table :test #'equal))
;; TODO: see `tramp-handle-abbreviate-file-name'
(cl-defun akn/tramp-home-dir (&optional (file (or buffer-file-name default-directory)))
  "Get the home directory on the host of FILE.

FILE defaults to the current `buffer-file-name'."
  (if (akn/tramp-p file)
      (progn
        (require 'tramp)
        (let ((home (with-parsed-tramp-file-name file parsed
                            (tramp-make-tramp-file-name parsed "~"))))
             (or (gethash home akn/tramp-home-dir--truename-cache)
                 (puthash home (file-truename (expand-file-name home)) akn/tramp-home-dir--truename-cache))))
    (file-truename "~")))

;;; font stuff
(defvar akn/original-font)
(defvar akn/preview--original-font)
(defvar akn/original-variable-pitch-font)
(defvar akn/preview--original-vp-font)

;;;###autoload
(defun akn/reload-font ()
  (interactive)
  (if (fboundp 'doom/reload-font)
      (doom/reload-font)
    (error "doom/reload-font not defined"))
  (when (and (bound-and-true-p mixed-pitch-mode)
             (fboundp 'mixed-pitch-mode))
    (mixed-pitch-mode -1)
    (mixed-pitch-mode 1)))

(defun akn/switch-font (font)
  (setq! doom-font font)
  (akn/reload-font))
;;;###autoload
(defun akn/choose-font-family (font-family)
  (interactive
   (list (akn/completing-read
          "Font name: "
          (and window-system
               (fboundp #'font-family-list)
               (font-family-list))
          :predicate nil
          :require-match t
          :default (if (or (stringp doom-font) (null doom-font))
                       doom-font
                     (symbol-name (font-get doom-font :family)))
          :preview-key "C-SPC"
          :state (lambda (action selected-font)
                   (pcase action
                     ('setup
                      (setq akn/preview--original-font doom-font))
                     ('preview
                      (ignore-errors
                        (akn/switch-font (or selected-font akn/preview--original-font)))))))))
  (akn/switch-font font-family))
;;;###autoload
(defun akn/reset-font ()
  (interactive)
  (akn/switch-font akn/original-font))

(defun akn/switch-variable-pitch-font (font)
  (setq! doom-variable-pitch-font font)
  (akn/reload-font))
;;;###autoload
(defun akn/choose-variable-pitch-font-family (font-family)
  (interactive
   (list (akn/completing-read
          "Font name: "
          (and window-system
               (fboundp #'font-family-list)
               (font-family-list))
          :predicate nil
          :require-match t
          :default (if (stringp doom-variable-pitch-font)
                       doom-variable-pitch-font
                     (symbol-name (font-get doom-variable-pitch-font :family)))
          :preview-key "C-SPC"
          :state (lambda (action selected-font)
                   (pcase action
                     ('setup
                      (setq akn/preview--original-vp-font doom-variable-pitch-font))
                     ('preview
                      (ignore-errors
                        (akn/switch-variable-pitch-font (or selected-font akn/preview--original-vp-font)))))))))
  (akn/switch-variable-pitch-font font-family))
;;;###autoload
(defun akn/reset-variable-pitch-font ()
  (interactive)
  (akn/switch-variable-pitch-font akn/original-variable-pitch-font))

;;; faces
(defun akn/local-set-faces! (&rest stuff)
  "Like `custom-set-faces!', but only applies locally."
  (declare (indent defun))
  (require 'face-remap)
  (cl-loop for args in stuff
           when args
           collect (face-remap-add-relative (car args) (cdr args))))

(defun akn/local-unset-faces! (cookies)
  "To undo the effect of an `akn/local-set-faces!', pass this
function the return value of `akn/local-set-faces!'."
  (require 'face-remap)
  (mapc 'face-remap-remove-relative cookies))

;;; opacity

(defun akn--doom-opacity-parameter ()
  (if (eq window-system 'pgtk)
      'alpha-background
    'alpha))
(defun akn--doom-opacity-lower-limit (&optional param)
  (if (eq (or param (akn--doom-opacity-parameter)) 'alpha)
      frame-alpha-lower-limit
    0))
(defun akn--prompt-opacity (&optional param)
  (read-number (format "Opacity (%s-100): " (akn--doom-opacity-lower-limit param))
               (akn/current-opacity param)))

(defun akn/opaque-p (&optional param)
  (= (akn/current-opacity param) 100))
(defun akn/transparent-p (&optional param)
  (not (akn/opaque-p param)))
(defun akn/current-opacity (&optional param)
  (min 100
       (or (frame-parameter nil (or param (akn--doom-opacity-parameter)))
           100)))

;;;autoload
(defun akn/doom/set-frame-opacity (opacity &optional frames)
  (interactive (list (akn--prompt-opacity) current-prefix-arg))
  (if (fboundp 'doom/set-frame-opacity)
      (funcall #'doom/set-frame-opacity opacity (or frames (list (selected-frame))))
    (error "doom/set-frame-opacity not defined")))
;;;###autoload
(defun akn/set-frame-opacity/alpha-background (opacity)
  "Change the current frame's opacity (but keeping foreground text opaque).

OPACITY is an integer between 0 to 100, inclusive."
  (interactive (list (akn--prompt-opacity 'alpha-background)))
  (set-frame-parameter nil 'alpha-background opacity))
;;;###autoload
(defun akn/set-frame-opacity/alpha (opacity)
  "Change the current frame's opacity, including foreground text opacity.

OPACITY is an integer between 0 to 100, inclusive."
  (interactive (list (akn--prompt-opacity 'alpha)))
  (set-frame-parameter nil 'alpha opacity))

(defvar akn/transparent-opacity 90)
(defun akn/transparency-on ()
  (interactive)
  (unless (akn/transparent-p)
    (akn/doom/set-frame-opacity akn/transparent-opacity)
    (message "Opacity is now %s%%" (akn/current-opacity))))
(defun akn/transparency-off ()
  (interactive)
  (when (akn/transparent-p)
    (setq akn/transparent-opacity (akn/current-opacity))
    (akn/doom/set-frame-opacity 100)
    (message "Opacity is now %s%%" (akn/current-opacity))))

;;;###autoload
(defun akn/toggle-opacity (&optional level)
  (interactive "P")
  (cond
   (level (if (numberp level)
              (akn/doom/set-frame-opacity level)
            (call-interactively #'akn/doom/set-frame-opacity))
          (message "Opacity is now %s%%" (akn/current-opacity)))
   ((akn/opaque-p) (akn/transparency-on))
   (t (akn/transparency-off))))
;;;###autoload
(defun akn/opacity-down (&optional amount)
  (interactive "P")
  (unless (eq (akn/current-opacity) 0)
    (akn/doom/set-frame-opacity (- (akn/current-opacity) (or amount 1))))
  (message "Opacity is now %s%%" (akn/current-opacity)))
;;;###autoload
(defun akn/opacity-up (&optional amount)
  (interactive "P")
  (unless (eq (akn/current-opacity) 100)
    (akn/doom/set-frame-opacity (+ (akn/current-opacity) (or amount 1))))
  (message "Opacity is now %s%%" (akn/current-opacity)))

;;; akn/count-chars
;; https://stackoverflow.com/a/73228375
(defun akn/count-chars (char str)
  (let ((s (char-to-string char))
        (count 0)
        (start-pos -1))
    (while (setq start-pos (string-search s str (+ 1 start-pos)))
      (setq count (+ 1 count)))
    count))

;;; making stuff act more like regular modes
;;;; make shift-select-mode act more like a regular mode
(defvar akn/shift-select-mode--true-value (or shift-select-mode t))
;;;###autoload
(defvaralias 'akn/shift-select-mode 'shift-select-mode)
;;;###autoload
(defun akn/shift-select-mode (&optional arg)
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           'toggle)))
  (when shift-select-mode (setq akn/shift-select-mode--true-value shift-select-mode))
  (when (null arg) (setq arg 1))
  (let ((enable (cond
                 ((eq arg 'toggle) (not akn/shift-select-mode))
                 ((null arg) nil)
                 ((numberp arg) (>= arg 0))
                 (t t))))
    (setq shift-select-mode
          (if enable
              akn/shift-select-mode--true-value
            nil)))
  (when (called-interactively-p 'interactive)
    (message "shift-select-mode = %s" shift-select-mode)))

;;;; akn/auto-save-visited-local-mode
;;;###autoload
(define-minor-mode akn/auto-save-visited-local-mode
  "Buffer-local version of `auto-save-visited-mode'."
  :group 'akn
  (when akn/auto-save-visited-local-mode
    (auto-save-visited-mode)))
(setq auto-save-visited-predicate
      (lambda () akn/auto-save-visited-local-mode))

;;;###autoload
(define-minor-mode akn/immediate-auto-save-visited-local-mode
  ""
  :group 'akn
  (akn/mode-set akn/immediate-auto-save-visited-local-mode
    auto-save-visited-interval 0.6)
  (akn/auto-save-visited-local-mode (if akn/immediate-auto-save-visited-local-mode +1 -1)))
;; keep after reverting
(add-hook 'after-change-major-mode-hook #'akn/immediate-auto-save-visited-local-mode-reenable-h)
(defun akn/immediate-auto-save-visited-local-mode-reenable-h ()
  (when akn/immediate-auto-save-visited-local-mode (akn/immediate-auto-save-visited-local-mode)))
(put 'akn/immediate-auto-save-visited-local-mode 'permanent-local t)

;;;; akn/line-move-visual-mode

;;;###autoload
(defvaralias 'akn/line-move-visual-mode 'line-move-visual)
;;;###autoload
(define-minor-mode akn/line-move-visual-mode
  "A minor mode reflecting the value of `line-move-visual'."
  :variable line-move-visual
  :keymap (make-sparse-keymap)
  :group 'akn)
;;;###autoload
(defun akn/line-move-visual-mode-set ()
  (akn/line-move-visual-mode
   (if akn/line-move-visual-mode 1 -1)))
(add-hook 'visual-line-mode-hook #'akn/line-move-visual-mode-set)
(add-hook 'after-change-major-mode-hook #'akn/line-move-visual-mode-set)

;;;; akn/mode-disabler

(defun akn/mode-disabler (mode)
  "Return the name of a function that turns off MODE."
  (let ((sym (akn/symbol-format "akn/turn-off-%s" mode)))
    (defalias sym
      (lambda (&rest _)
        (interactive)
        (funcall mode -1))
      (format "Turn off %s.

This function was created by `akn/mode-disabler'." mode))
    sym))

;;;; locally disabling smooth scroll

(defvar-keymap akn/local-smooth-scroll-disabled-mode-map
  "<remap> <pixel-scroll-precision>"        #'mwheel-scroll
  "<remap> <pixel-scroll-start-momentum>"   #'ignore
  "<remap> <pixel-scroll-interpolate-down>" #'ignore
  "<remap> <pixel-scroll-interpolate-up>"   #'ignore)

;;;###autoload
(define-minor-mode akn/local-smooth-scroll-disabled-mode
  "Disable smooth scrolling."
  :group 'akn
  :map akn/local-smooth-scroll-disabled-mode-map
  (akn/mode-set akn/local-smooth-scroll-disabled-mode
    mac-mouse-wheel-smooth-scroll nil
    pixel-scroll-precision-use-momentum nil
    scroll-conservatively 10
    scroll-margin 0
    ultra-scroll-mode nil
    pixel-scroll-precision-mode nil
    mwheel-coalesce-scroll-events t))
(with-eval-after-load 'ultra-scroll
  (akn/prioritize-minor-mode-keymap 'akn/local-smooth-scroll-disabled-mode))

;;; akn/region-string
(defmacro akn/region-string ()
  `(buffer-substring (region-beginning) (region-end)))

;;; akn/deep-unpropertize-strings

;;;###autoload
(defun akn/deep-unpropertize-strings (stuff)
  "Unpropertize all strings within the object STUFF.

Recurses inner lists to find strings to unpropertize."
  (cond
   ((stringp stuff) (substring-no-properties stuff))
   ((proper-list-p stuff) (mapcar #'akn/deep-unpropertize-strings stuff))
   ((consp stuff)
    (cons (akn/deep-unpropertize-strings (car stuff))
          (akn/deep-unpropertize-strings (cdr stuff))))
   (t stuff)))

;;; window management

;; (defalias 'akn/this-window #'get-buffer-window)
(defalias 'akn/this-window #'selected-window)
(defalias 'akn/this-buffer #'current-buffer)
(defalias 'akn/this-frame #'selected-frame)
(defmacro akn/this-process (&optional buffer)
  `(get-buffer-process ,(if buffer
                            `(or ,buffer (current-buffer))
                          `(current-buffer))))

;;;###autoload
(defun akn/record-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (cl-pushnew (current-buffer) (frame-parameter nil 'buffer-list) :test #'eq)
    ;; window.c says: switch-to-buffer uses (select-window (selected-window)) as a
    ;; "clever" way to call record_buffer from Elisp
    (select-window (selected-window))))
;;;###autoload
(defun akn/mark-buffer-real (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local doom-real-buffer-p t)
    (akn/record-buffer)))
;;;###autoload
(defun akn/mark-buffer-unreal (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local doom-real-buffer-p nil)))

;;;; join (unfill) paragraph
;;;###autoload
(defun akn/join-paragraphs ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively #'fill-paragraph)))
;;;###autoload
(defalias 'akn/unfill-paragraphs #'akn/join-paragraphs)

;;;; debug
;;;###autoload
(defun akn/debug ()
  (interactive)
  (setq debug-on-error t))
;;;###autoload
(defun akn/undebug ()
  (interactive)
  (setq debug-on-error nil)
  (top-level)
  (cancel-debug-watch)
  (cancel-debug-on-entry)
  (when (and (bound-and-true-p doom-debug-mode)
             (fboundp 'doom-debug-mode))
    (doom-debug-mode -1)))
;;;###autoload
(defun akn/toggle-doom-log ()
  (interactive)
  (if (setq doom-inhibit-log (not doom-inhibit-log))
      (message "Logging disabled")
    (message "Logging enabled")))

;;; focus hooks

(defvar akn/emacs-focus-in-hook nil)
(defvar akn/emacs-focus-out-hook nil)
(defvar akn/emacs-focus-change-hook nil)

(defvar akn--emacs-focused-p 'startup)

(defun akn--emacs-focused-p ()
  (cl-loop for frame being the frames
           thereis (frame-focus-state frame)))

(add-function :after after-focus-change-function
              #'akn--after-focus-change-function)
(defvar akn--emacs-focus-timer nil)
(defvar akn/emacs-focus-timer-delay 0.06)
(defun akn--after-focus-change-function ()
  (if akn--emacs-focus-timer
      (timer-set-idle-time akn--emacs-focus-timer akn/emacs-focus-timer-delay)
    (setq akn--emacs-focus-timer
          (run-with-idle-timer akn/emacs-focus-timer-delay nil #'akn--after-focus-change-function--run))))

(defun akn--after-focus-change-function--run ()
  (setq akn--emacs-focus-timer nil)
  (let ((was-focused akn--emacs-focused-p))
    (setq akn--emacs-focused-p (akn--emacs-focused-p))
    (unless (eq was-focused 'startup)
      (if akn--emacs-focused-p
          (when (not was-focused) (run-hooks 'akn/emacs-focus-in-hook 'akn/emacs-focus-change-hook))
        (when was-focused (run-hooks 'akn/emacs-focus-out-hook 'akn/emacs-focus-change-hook))))))

;;; helper for starting processes

(defvar akn/process)

(defun akn/popup-buffer (&optional buffer-or-name &rest options)
  (let ((buf (cond
              ((null buffer-or-name)
               (generate-new-buffer "my-buffer"))
              ((stringp buffer-or-name)
               (get-buffer-create buffer-or-name))
              (t
               buffer-or-name)))
        (+popup-defaults (append options (bound-and-true-p +popup-defaults))))
    (akn/mark-buffer-real buf)
    (save-selected-window
      (if (fboundp '+popup/buffer)
          (with-current-buffer buf
            (+popup/buffer))
        (display-buffer buf)))
    (when-let* ((select (plist-get +popup-defaults :select)))
      (if (eq select t)
          (select-window (get-buffer-window buf))
        (funcall select (get-buffer-window buf) (selected-window))))
    buf))
(defmacro akn/popup-new-buffer (&optional buffer-name &rest options)
  `(akn/popup-buffer (generate-new-buffer (or ,buffer-name "my-buffer")) ,@options))

(defun akn/format-cmd (format-string &rest args)
  (apply #'format format-string (mapcar #'shell-quote-argument args)))

(defmacro akn/maybe-with-editor (&rest args)
  `(if (progn (require 'with-editor nil t)
              (fboundp 'with-editor))
       (with-editor ,@args)
     (progn ,@args)))

(cl-defun akn/run-command (command
                           &key
                           sync
                           (name
                            (or (and (stringp command) command)
                                (and (stringp (car-safe command)) (car command))
                                "my-command"))
                           shell
                           (on-finish #'ignore) (on-stopped-but-continuable #'ignore) (on-output #'ignore)
                           output-buffer stderr
                           input
                           (connection-type 'pipe)
                           boring)
  "Run a command, either synchronously or asynchronously.

This is a convenient wrapper around `call-process-region' or
`make-process'.

COMMAND is either a string (which will be split into a list of
arguments if necessary) or a list of strings (which will be
escaped and joined by spaces if necessary). The `akn/format-cmd'
function may be helpful for generating this.

By default, the command will be asynchronous and immediately
return the process. But if SYNC is non-nil, it will be run
synchronously, and it will return a cons
cell (EXIT-STATUS-OR-SIGNAL-DESCRIPTION-STRING . OUTPUT-STRING).

NAME is the name of the process. This is ignored if the command
is run SYNC.

By default, the COMMAND will be run directly, but if SHELL is
non-nil, it will be run through a shell, similarly to
`start-process-shell-command' (if async) or `shell-command' (if
SYNC).

ON-FINISH is called with one argument, FAIL, when the command finishes.
If the command is successful, FAIL is nil.
If the command fails, FAIL is a numeric exit code, a signal
 description string, or the symbol `deleted'.

ON-STOPPED-BUT-CONTINUABLE is called if the process was
stopped (as with `stop-process') but can be continued with
`continue-process'.

ON-OUTPUT is called whenever the command outputs anything. Unless
STDERR is specified, this includes both standard error and
standard output.

OUTPUT-BUFFER can be a buffer or a buffer name string. By
default, it won't be displayed. If you want it to be displayed,
use `akn/popup-buffer'.

(TODO: option to support comint like compilation-mode)
(TODO: option to pop-up buffer)

If STDERR is non-nil, standard error will not be sent to
ON-OUTPUT or the OUTPUT-BUFFER. If it's a function, the function
will be passed all standard error. If it's a buffer, standard
error will be appended to that buffer.

INPUT is a string that is immediately passed as standard input to
the process, followed by EOF. Alternatively, if the process is
async, you can use the functions `process-send-string',
`process-send-region', and `process-send-eof' on the process
returned by this function.

CONNECTION-TYPE is the control type of device used to communicate with
subprocesses. It's ignored if the command is run SYNC. Unlike in
`make-process', this always defaults to `pipe'. Values are `pipe' to use
a pipe, `pty' to use a pty, or nil to use the default specified through
`process-connection-type'. If TYPE is a cons (INPUT . OUTPUT), then
INPUT will be used for standard input and OUTPUT for standard output
(and standard error if :stderr is nil). See info node `(elisp)
Asynchronous Processes'. Also see URL
https://emacs.stackexchange.com/a/40630.

By default, when exiting Emacs, if this process is running,
you'll be asked if it's okay to kill it. If BORING is non-nil,
Emacs will kill this process without querying."
  (when (and (listp command))
    ;; so that command can be, for example, (list (split-string-shell-command "ls -a") default-directory),
    ;; and that'll expand out to (list "ls" "-a" default-directory)
    (setq command (flatten-list command)))
  ;; (akn/maybe-with-editor)
  (let ((the-command (cond
                      (shell
                       (list (if (stringp shell) shell shell-file-name)
                             shell-command-switch
                             (if (stringp command)
                                 command
                               (string-join (mapcar #'shell-quote-argument command)
                                            " "))))
                      ((listp command)
                       command)
                      ((stringp command)
                       (split-string-shell-command command))
                      (t (error "akn/run-command: invalid COMMAND argument: %S" command)))))
    (cond
     (sync
      (when stderr (error "akn/run-command: STDERR argument not yet supported for SYNC commands"))
      (with-temp-buffer
        (or output-buffer (setq output-buffer (current-buffer)))
        (when (stringp output-buffer)
          (setq output-buffer (get-buffer-create output-buffer)))
        (with-current-buffer output-buffer
          (let ((res (apply #'call-process-region
                            (or input (point)) (point) ;START END
                            (car the-command)          ;PROGRAM
                            nil                        ;DELETE
                            (list output-buffer t)     ;BUFFER
                            nil                        ;DISPLAY
                            (cdr the-command))))       ;ARGS
            (funcall on-output (buffer-string))
            (funcall on-finish (if (equal res 0) nil res))
            (cons res (buffer-string))))))
     (t
      (let (the-process)
        (setq
         the-process
         (make-process :name name
                       :command the-command
                       :buffer output-buffer
                       :stderr (cond
                                ((functionp stderr)
                                 (make-pipe-process :name (concat name "-stderr-pipe-process")
                                                    :filter (lambda (_proc string)
                                                              (let ((akn/process the-process))
                                                                (funcall stderr string)))
                                                    :noquery t))
                                (stderr stderr))
                       :noquery boring
                       :connection-type connection-type
                       :filter (lambda (proc string)
                                 (let ((akn/process proc))
                                   (funcall on-output string)
                                   (when (buffer-live-p (process-buffer proc))
                                     (with-current-buffer (process-buffer proc)
                                       (let ((moving (= (point) (process-mark proc))))
                                         (save-excursion
                                           ;; Insert the text, advancing the process marker.
                                           (goto-char (process-mark proc))
                                           (let ((inhibit-read-only t))
                                             (insert (if (fboundp 'xterm-color-filter) (xterm-color-filter string) string))
                                             (set-marker (process-mark proc) (point))))
                                         (if moving (goto-char (process-mark proc))))))))
                       :sentinel (lambda (proc event)
                                   (let ((akn/process proc)
                                         (status (process-status proc))
                                         (exit-status (process-exit-status proc)))
                                     (pcase event
                                       ((let 'stop status)
                                        (funcall on-stopped-but-continuable))
                                       ("finished\n"
                                        (funcall on-finish nil))
                                       ("deleted\n"
                                        (funcall on-finish 'deleted))
                                       ((rx (or "exited abnormally with code " "failed with code "))
                                        (funcall on-finish exit-status))
                                       ((guard (and (numberp exit-status) (not (equal exit-status 0))))
                                        (funcall on-finish exit-status))
                                       ((let (or 'signal 'failed) status)
                                        (funcall on-finish exit-status))
                                       ((let (or 'exit 'closed) status)
                                        (funcall on-finish nil)))))))
        (cond
         ((stringp input)
          (process-send-string the-process input)
          (process-send-eof the-process))
         (input
          (error "akn/run-command: invalid INPUT argument: %S" input)))
        the-process)))))

;;; akn/cmds!

(defmacro akn/cmds! (&rest branches)
  "Returns a dispatcher that runs the a command in BRANCHES.
Meant to be used as a target for keybinds (e.g. with `define-key' or `map!').

BRANCHES is a flat list of CONDITION COMMAND pairs. CONDITION is a lisp form
that is evaluated when (and each time) the dispatcher is invoked. If it returns
non-nil, COMMAND is invoked, otherwise it falls through to the next pair.

The last element of BRANCHES can be a COMMANd with no CONDITION. This acts as
the fallback if all other conditions fail.

Otherwise, Emacs will fall through the keybind and search the next keymap for a
keybind (as if this keybind never existed).

See `general-key-dispatch' for what other arguments it accepts in BRANCHES."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (pcase-lambda (`(,pred ,def))
                                       (when (not (or (symbolp (akn/unquote def)) (memq (car-safe def) '(lambda cmd! akn/defun akn/cmds!))))
                                         (error "akn/cmds!: %S was expected to be a sharp-quoted function" def))
                                       `((setq it (with-demoted-errors "error in akn/cmds! condition: %S" ,pred))
                                         ,def))
                                     defs)
                           (t ,fallback))))))))

;;; polymode

(defun akn/polymode-p (&optional buffer)
  (condition-case nil
      (buffer-local-value 'polymode-mode (or buffer (current-buffer)))
    (void-variable nil)))

;;; doom version

(defun akn/doom-commithash ()
  (pcase-let* ((default-directory doom-emacs-dir)
               (process-file-side-effects nil)
               (`(,status . ,output) (akn/run-command "git rev-parse HEAD" :sync t :boring t)))
    (when (equal status 0)
      (string-trim output))))

(defun akn/get-doom-latest-commithash (callback)
  (url-retrieve "https://api.github.com/repos/doomemacs/doomemacs/commits?per_page=1"
                (lambda (status)
                  (forward-paragraph)
                  (funcall callback
                           (and (not (plist-get status :error))
                                (akn/-> (json-parse-buffer)
                                        (seq-first)
                                        (map-elt "sha")))))
                nil 'silent 'inhibit-cookies))

(defun akn/get-doom-up-to-date-p (callback)
  (let ((current-commithash (akn/doom-commithash)))
    (akn/get-doom-latest-commithash
     (lambda (latest-commithash)
       (funcall callback
                (cond
                 ((null current-commithash) 'repo-commithash-failed)
                 ((null latest-commithash) 'fetch-failed)
                 ((equal current-commithash latest-commithash) 'up-to-date)
                 (t
                  (message "%S vs. %S" current-commithash latest-commithash)
                  nil)))))))

;; (akn/get-doom-up-to-date-p (lambda (x) (message "ret=%S" x)))

;;; thingatpt stuff

(defun akn/get-forward-thing-function (thing)
  (require 'thingatpt)
  (defvar forward-thing-provider-alist)
  (if (assq thing forward-thing-provider-alist)
      (lambda (&optional n) (forward-thing thing n))
    (or (get thing 'forward-op)
        (intern-soft (format "forward-%s" thing))
        (error "Can't determine how to move over a %s" thing))))

(defun akn/transpose-thing (thing arg)
  (transpose-subr (akn/get-forward-thing-function thing) arg))

(defun akn/drag-thing-forward (thing arg)
  (let ((offset (- (point)
                   (save-excursion (forward-thing thing) (point)))))
    (akn/transpose-thing thing arg)
    (forward-char offset)))

(defun akn/drag-thing-backward (thing arg)
  (akn/drag-thing-forward thing (- (or arg 1))))

;;;; akn-defun thing

;;;###autoload
(cl-defun forward-akn-defun (&optional (count 1))
  (interactive "^p")
  (if (>= count 0)
      (end-of-defun count)
    (beginning-of-defun (- count))))
;;;###autoload
(cl-defun forward-akn-defun-comments (&optional (count 1))
  (interactive "^p")
  (if (>= count 0)
      (end-of-defun count)
    (beginning-of-defun-comments (- count))))
;;;###autoload
(cl-defun backward-akn-defun (&optional (count 1))
  (interactive "^p")
  (forward-akn-defun (- count)))
;;;###autoload
(cl-defun backward-akn-defun-comments (&optional (count 1))
  (interactive "^p")
  (forward-akn-defun-comments (- count)))

(put 'akn-defun 'beginning-op #'beginning-of-defun)
(put 'akn-defun 'end-op       #'end-of-defun)
(put 'akn-defun 'forward-op   #'forward-akn-defun)

(put 'akn-defun-comments 'beginning-op #'beginning-of-defun-comments)
(put 'akn-defun-comments 'end-op       #'end-of-defun)
(put 'akn-defun-comments 'forward-op   #'forward-akn-defun-comments)

;; TODO: still not quite right, swap vs. move
;;;autoload
(defun akn/drag-defun-up (arg)
  (interactive "*p")
  (akn/drag-thing-backward 'akn-defun-comments arg))
;;;###autoload
(defun akn/transpose-defuns (arg)
  (interactive "*p")
  (akn/transpose-thing 'akn-defun-comments arg))
;;;###autoload
(defun akn/drag-defun-down (arg)
  (interactive "*p")
  (akn/drag-thing-forward 'akn-defun-comments arg))

;;;; akn-paragraph thing

;;;###autoload
(defun forward-akn-paragraph (&optional count)
  (interactive "^p")
  (setq count (or count 1))
  (cond
   ((> count 0)
    (cl-loop repeat count
             until (eobp)
             do (end-of-paragraph-text)
             finally (end-of-line)))
   ((< count 0)
    (cl-loop repeat (- count)
             until (bobp)
             do (start-of-paragraph-text)
             finally (beginning-of-line)))))
;;;###autoload
(defun backward-akn-paragraph (&optional count)
  (interactive "^p")
  (forward-akn-paragraph (- (or count 1))))

;;;###autoload
(defun akn/transpose-paragraphs (arg)
  (interactive "*p")
  (akn/transpose-thing 'akn-paragraph arg))
;;;###autoload
(defun akn/drag-paragraph-down (arg)
  (interactive "*p")
  (akn/drag-thing-forward 'akn-paragraph arg))
;;;###autoload
(defun akn/drag-paragraph-up (arg)
  (interactive "*p")
  (akn/drag-thing-backward 'akn-paragraph arg))

;;; provide
(provide 'akn)
;;; akn.el ends here

;; Local Variables:
;; package-lint--sane-prefixes: "\\`akn[/-]"
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not make-local)
;; End:
