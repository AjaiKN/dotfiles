;; -*- no-byte-compile: t; eval: (+word-wrap-mode -1); -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'. (or SPACE h r r)

;; Use `doom/bump-package-at-point' to update the pinned commit for a package,
;; or to pin a package if it's not pinned.
;; Also see `doom/bump-packages-in-buffer' (which doesn't seem to work right for adding a pin to an unpinned package).

;; Local packages use this syntax, with the path relative to ~/.config/doom/lisp/:
;;    :recipe (:local-repo "path")

;;; pinning ahead
;; stuff where I want to use a newer version than Doom goes here
;; https://github.com/doomemacs/doomemacs/issues/8287
(when (modulep! :lang coq)
  (unpin! proof-general))
(when (modulep! :lang graphviz)
  (package! graphviz-dot-mode :pin "516c151b845a3eb2da73eb4ee648ad99172087ac"))

(package! track-changes :built-in t)
(package! eldoc :built-in t)
(when (modulep! :tools pdf)
  (package! pdf-tools :built-in 'prefer)) ;https://discourse.nixos.org/t/how-can-i-get-emacs-pdf-tools-working/10270

;;; new packages
;; (package! ____________________ :pin "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :disable ________________________________________ :recipe ___)
;;;; Required
(package! shut-up :type 'core     :pin "ed62a7fefdf04c81346061016f1bc69ca045aaf6") ;also see fork https://github.com/jamescherti/be-quiet.el
(package! akn  :type 'core     :recipe (:local-repo "lisp/akn")) ;:build (:not compile)))
(package! boring-processes :recipe (:local-repo "lisp/boring-processes")) ;TODO: shouldn't be required
(package! el-patch :pin "5adb7097d0ff3d9e004a8bb07c0b25f7ee20ba8a")
;;;; Making repos local
(when (file-exists-p "~/prog/emacs/minizinc-ts-mode")
  (package! minizinc-ts-mode :recipe (:local-repo "~/prog/emacs/minizinc-ts-mode")))
(when (file-exists-p  "~/prog/roc/roc-ts-mode")
  (package! roc-ts-mode :recipe (:local-repo "~/prog/roc/roc-ts-mode")))
;;;; Conditional
(package! transient-showcase      :pin "ac2bbe6a6be5f7c8f2251517d50410991db66cfa" :disable (not (modulep! :lang emacs-lisp))        :recipe (:host github :repo "positron-solutions/transient-showcase"))
(package! page-break-lines        :pin "84b872a056404f11cacbf3fc2b0788aa7feac635" :disable (not (or (modulep! :lang cc) (modulep! :lang emacs-lisp))))
(package! elisp-indent-docstrings                   :disable (not (modulep! :lang emacs-lisp))        :recipe (:local-repo "lisp/elisp-indent-docstrings"))
(package! valign                  :pin "8b0002844cb6012ac4f8952e255c165e0e3be5b6" :disable (not (modulep! :lang org)))
(package! org-wc                  :pin "dbbf794e4ec6c4080d945f43338185e34a4a582d" :disable (not (modulep! :lang org)))
(package! org-appear              :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09" :disable (not (modulep! :lang org)))
(package! org-super-agenda        :pin "fb20ad9c8a9705aa05d40751682beae2d094e0fe" :disable (not (modulep! :lang org)))
(package! lsp-tailwindcss         :pin "8574cb3ad2e669eebb34b4d57c3cdef5a424a9b5" :disable (not (modulep! :lang web))               :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! astro-ts-mode           :pin "78e7e942011839bd4f4de0a1d8460f5879ba4ca5" :disable (not (modulep! :lang web)))
(package! launchctl               :pin "c9b7e93f5ec6fa504dfb03d60571cf3e5dc38e12" :disable (not (modulep! :os macos)))
(package! applescript-mode        :pin "3dbbb8b48e519a5208ce237db577056c7a5a5943" :disable (not (modulep! :os macos)))
(package! eshell-vterm            :pin "20f4b246fa605a1533cdfbe3cb7faf31a24e3d2e" :disable (not (and (modulep! :term eshell) (modulep! :term vterm))))
(package! coterm                  :pin "6f04b2e7151db862c85b1cbdcf523bdeae27c006" :disable (not (modulep! :term shell)))
(package! lsp-booster             :pin "5f702a2699f306a3958ff1996a2b1a625f0cee0b" :recipe (:host github :repo "blahgeek/emacs-lsp-booster"))
(package! eglot-booster           :pin "1260d2f7dd18619b42359aa3e1ba6871aa52fd26" :disable (not (modulep! :tools lsp +eglot))       :recipe (:host github :repo "jdtsmith/eglot-booster"))
(package! magit-delta             :pin "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616" :disable (not (modulep! :tools magit)))
(package! magit-todos             :pin "bd27c57eada0fda1cc0a813db04731a9bcc51b7b" :disable (not (modulep! :tools magit)))
;;;; :ui
(package! color-identifiers-mode  :pin "162e56caa183c16f252b3e0a9dc816e5baaaea28")
(package! framemove               :pin "0faa8a4937f398e4971fc877b1c294100506b645" :disable nil                                      :recipe (:host github :repo "emacsmirror/framemove"))
(package! hercules                :pin "557da39878d0637395fdded91243b340c37eff7b" :disable nil)
(package! evil-quickscope :recipe (:local-repo "lisp/evil-quickscope") :disable (or t (not (modulep! :editor evil))))
(package! rainbow-mode            :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c" :disable nil)
(package! vdiff                   :pin "170e968c6a46a572b30c52c1b038232d418734cc" :disable nil)
(package! xterm-color             :pin "2ad407c651e90fff2ea85d17bf074cee2c022912" :disable nil)
(package! scrollkeeper            :pin "3c4ac6b6b44686d31c260ee0b19daaee59bdccd6" :disable t)
(package! sticky-scroll-mode      :pin "0771266fc8ae643a3ab71e62b4c955169f5388ed" :disable t                                      :recipe (:host github :repo "jclasley/sticky-scroll-mode"))
(package! yequake                 :pin "0771266fc8ae643a3ab71e62b4c955169f5388ed" :disable t)
(package! frog-jump-buffer        :pin "0771266fc8ae643a3ab71e62b4c955169f5388ed" :disable t)
(package! alert                   :pin "0771266fc8ae643a3ab71e62b4c955169f5388ed" :disable t)
;;;; :editor
(package! copy-as-format          :pin "b9f6f725ca9701c5a02bfb479573fdfcce2e1e30" :disable nil)
(package! drag-stuff              :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8" :disable nil) ; no longer built-in in Doom for evil users
(package! evil-evilified-state :recipe (:local-repo "lisp/evil-evilified-state"))
(package! expand-region           :pin "351279272330cae6cecea941b0033a8dd8bcc4e8" :disable nil) ; not necessarily built-in in Doom for evil users
(package! speedrect               :pin "ec3e08544494838e0e6c476d87966f14180bd5e4" :disable nil :recipe (:host github :repo "jdtsmith/speedrect"))
(package! meow                    :pin "c0985b3531ca14067d96ed3018bec80fe7eeb7db" :disable t) ; only using for meow-keypad right now
(package! hardtime                :pin "8bbc54f8d76c012dfbb619bd0fdd607cb23f4697" :disable t :recipe (:host github :repo "ichernyshovvv/hardtime.el"))
(package! akn-keypad :recipe (:local-repo "lisp/akn-keypad") :disable t) ;:build (:not compile)))
;;;; :emacs
(package! git-auto-commit-mode :recipe (:local-repo "lisp/git-auto-commit-mode"))
(package! better-backup :recipe (:local-repo "lisp/better-backup"))
(package! org-week-table :recipe (:local-repo "lisp/org-week-table"))


;; (package! corfu-mouse :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-mouse.git") :pin "8301e8c905828158bc084d9e6e3fb8c03ec0a005")
;; (package! outline-minor-faces)
;; (package! ruby-end)
;; (package! scroll-restore)
;; (package! bm)
;; (package! nano :recipe (:host github :repo "rougier/nano-emacs"))

;; https://alhassy.com/repl-driven-development, https://melpa.org/#/repl-driven-development, https://emacsconf.org/2023/talks/eval/
;; https://emacsconf.org/2023/talks/repl/

;;https://github.com/arthurcgusmao/restore-point

;;; original comments

;; also see https://github.com/doomemacs/doomemacs/issues/3842

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)
;;(unpin! racket-mode)
