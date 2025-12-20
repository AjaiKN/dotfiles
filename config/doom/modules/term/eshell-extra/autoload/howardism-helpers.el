;;; term/eshell-extra/autoload/howardism-helpers.el -*- lexical-binding: t; -*-

;; https://howardism.org/Technical/Emacs/eshell-why.html
;; https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org (CC0)

(require 'dash)
(require 's)

;;;###autoload
(defun +eshell-fn-on-files (fun1 fun2 args)
  "Call FUN1 on the first element in list, ARGS.
Call FUN2 on all the rest of the elements in ARGS."
  (unless (null args)
    (let ((filenames (flatten-list args)))
      (funcall fun1 (car filenames))
      (when (cdr filenames)
        (mapc fun2 (cdr filenames))))
    ;; Return an empty string, as the return value from `fun1'
    ;; probably isn't helpful to display in the `eshell' window.
    ""))

;;;###autoload
(defun +eshell-command-to-string (command)
  "Return results of executing COMMAND in an eshell environtment.
The COMMAND can either be a string or a list."
  (when (listp command)
    ;; Since `eshell-command' accepts a string (and we want all its
    ;; other goodies), we synthesize a string, but since `command'
    ;; could be a parsed list, we quote all of the arguments.
    ;;
    ;; Hacky. Until I figure out a better way to call eshell,
    ;; as `eshell-named-command' doesn't work reliably:
    (setq command (s-join " " (cons (first command)
                                    (--map (format "\"%s\"" it) (rest command))))))
  (with-temp-buffer
    (eshell-command command t)
    (buffer-string)))

;;;###autoload
(defun +eshell-getopts (defargs args)
  "Return hash table of ARGS parsed against DEFARGS.
Where DEFARGS is an argument definition, a list of plists.
For instance:
   '((:name number :short \"n\"                 :parameter integer :default 0)
     (:name title  :short \"t\" :long \"title\" :parameter string)
     (:name debug  :short \"d\" :long \"debug\"))

If ARGS, a list of _command line parameters_ is something like:

    '(\"-d\" \"-n\" \"4\" \"--title\" \"How are that\" \"this\" \"is\" \"extra\")

The hashtable return would contain these entries:

    debug t
    number 4  ; as a number
    title \"How are that\" ; as a string
    parameters (\"this\" \"is\" \"extra\") ; as a list of strings "
  (let ((retmap    (make-hash-table))
        (short-arg (rx string-start "-" (group alnum)))
        (long-arg  (rx string-start "--" (group (1+ any)))))

    ;; Let's not pollute the Emacs name space with tiny functions, as
    ;; well as we want these functions to have access to the "somewhat
    ;; global variables", `retmap' and `defargs', we use the magical
    ;; `cl-labels' macro to define small functions:

    (cl-labels ((match-short (str defarg)
                  ;; Return t if STR matches against DEFARG's short label:
                  (and (string-match short-arg str)
                       (string= (match-string 1 str)
                                (plist-get defarg :short))))

                (match-long (str defarg)
                  ;; Return t if STR matches against DEFARG's long label:
                  (and (string-match long-arg str)
                       (string= (match-string 1 str)
                                (plist-get defarg :long))))

                (match-arg (str defarg)
                  ;; Return DEFARG if STR matches its definition (and it's a string):
                  (when (and (stringp str)
                             (or (match-short str defarg)
                                 (match-long str defarg)))
                    defarg))

                (find-argdef (str)
                  ;; Return entry in DEFARGS that matches STR:
                  (first (--filter (match-arg str it) defargs)))

                (process-args (arg parm rest)
                  (when arg
                    (let* ((defarg (find-argdef arg))
                           (key    (plist-get defarg :name)))
                      (cond
                       ;; If ARG doesn't match any definition, add
                       ;; everything else to PARAMETERS key:
                       ((null defarg)
                        (puthash 'parameters (cons arg rest) retmap))

                       ((plist-get defarg :help)
                        (error (documentation (plist-get defarg :help))))

                       ;; If argument definition has a integer parameter,
                       ;; convert next entry as a number and process rest:
                       ((eq (plist-get defarg :parameter) 'integer)
                        (puthash key (string-to-number parm) retmap)
                        (process-args (cadr rest) (caddr rest) (cddr rest)))

                       ;; If argument definition has a parameter, use
                       ;; the next entry as the value and process rest:
                       ((plist-get defarg :parameter)
                        (puthash key parm retmap)
                        (process-args (cadr rest) (caddr rest) (cddr rest)))

                       ;; No parameter? Store true for its key:
                       (t
                        (puthash key t retmap)
                        (process-args (first rest) (second rest) (cdr rest))))))))

      (process-args (first args) (second args) (cdr args))
      retmap)))
