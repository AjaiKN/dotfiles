;;; +modules.el -*- lexical-binding: t; -*-

;; NOTE: Machine-specific settings and `akn/tags' should be placed in local.el (gitignored).

(defvar akn/tags '(*basic)
  "A list of tags whose modules should be enabled.

By default, only modules marked `*basic' are enabled. If you add
`*extra' to the list, then the modules with that tag are enabled too.

To enable a one-off module, you can also just add the name of the
module (e.g. `evil' or `minimap') to `akn/tags'.

`akn/tags' should be modified in $DOOMDIR/local.el, which is gitignored
and therefore local to the current machine.")
(or (load! "./local" nil t)
    (load! "./+local-tags" nil t))

(defun akn/use-module-p (group module tags &optional _flags)
  "If this returns non-nil, the given module should be enabled.

Currently, I'm making this decision based on tags. If a module is marked
with any of the tags in `akn/tags' (or if `akn/tags' contains the name
of the module itself), then enable the module. Otherwise, don't."
  (require 'cl-lib)
  (cl-intersection (cons group (cons module tags)) akn/tags))

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib)
  (defun akn--doom-module-flagp (symbol)
    (and (symbolp symbol)
         (string-match-p (rx bos "+") (symbol-name symbol))))
  (defun akn--doom-module-tagp (symbol)
    (and (symbolp symbol)
         (string-match-p (rx bos "*") (symbol-name symbol))))
  (defun akn--doom-module! (group arg tags)
    (pcase arg
      ((and module (pred symbolp) (pred (not keywordp)) (pred (not akn--doom-module-flagp)))
       `(:if (akn/use-module-p ',group ',module ',tags)
            ,arg))
      (`(,(and kwd (or :if :when)) ,cond . ,body)
       `(,kwd ,cond
         ,@(cl-loop for thing in body
                    collect (akn--doom-module! group thing tags))))
      (`(,(and module (pred symbolp) (pred (not keywordp)) (pred (not akn--doom-module-flagp)))
         . ,(and flags (pred (seq-every-p #'akn--doom-module-flagp _))))
       `(:if (akn/use-module-p ',group ',module ',tags ',flags)
            ,arg))
      (_ (error "akn/doom!: unrecognized form: %S" arg)))))

(defmacro akn/doom! (&rest args)
  "Similar to the regular `doom!' macro, but with some features making it
easier to enable different modules on different machines:

1. If you add a symbol starting with an asterisk before a module (e.g.,
`*basic'), that symbol is a tag that gets assigned to the module.

2. A module only gets enabled if `akn/use-module-p', defined above,
returns true."
  (cons 'doom!
        (cl-loop with group = nil
                 with tags = nil
                 for arg in args
                 if (akn--doom-module-tagp arg)
                 do (push arg tags)
                 else collect
                 (prog1
                     (if (keywordp arg)
                         (setq group arg)
                       (akn--doom-module! group arg tags))
                   (setq tags nil)))))

(akn/doom! :input
           bidi              ; (tfel ot) thgir etirw uoy gnipleh
           chinese
           japanese
           layout            ; auie,ctsrnm is the superior home row
           *extra reverse-im
           *extra csi-u

           :completion
           (company +childframe)           ; the ultimate code completion backend
           *basic (corfu +orderless +icons +dabbrev)  ; complete with cap(f), cape and a flying feather!
           *native (fuzzy +fuz-bin +all)
           *basic hippie
           helm              ; the *other* search engine for love and life
           ido               ; the other *other* search engine...
           ivy               ; a search engine for love and life
           *basic (vertico +icons)             ; the search engine of the future
           *extra p-search
           *extra (:if (versionp! emacs-version >= "30") preview)

           :ui
           *extra blamer
           *basic buffer-move
           deft              ; notational velocity for Emacs
           *basic doom              ; what makes DOOM look the way it does
           *basic doom-dashboard    ; a nifty splash screen for Emacs
           emacs-dashboard    ; another nifty splash screen for Emacs
           doom-quit         ; DOOM quit-message prompts when you quit Emacs
           (emoji +unicode)  ; 🙂
           *extra extra-themes
           *extra golden-ratio
           *basic highlight-numbers
           *extra highlight-symbol
           *extra hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
           hydra
           *extra indent-guides     ; highlighted indent columns
           *extra (ligatures +extra)         ; ligatures and symbols to make your code pretty again
           mini-frame ; minibuffer in child frame
           *extra minimap           ; show a map of the code on the side
           *basic (modeline)          ; snazzy, Atom-inspired modeline, plus API
           *basic modeline-minor-modes
           *extra modeline-scrollbar
           *basic nav-flash         ; blink cursor line after big motions
           neotree           ; a project drawer, like NERDTree for vim
           *basic ophints           ; highlight the region an operation acts on
           *basic (popup +defaults)   ; tame sudden yet inevitable temporary windows
           *basic (smooth-scroll) ; +interpolate     ; So smooth you won't believe it's not butter
           *extra prism
           *basic rainbow-delimiters
           *extra read-aloud
           *basic tabs              ; a tab bar for Emacs
           ;; *basic treemacs          ; a project drawer, like neotree but cooler
           unicode           ; extended unicode support for various languages
           *basic (vc-gutter +pretty +diff-hl) ; vcs diff in the fringe
           vi-tilde-fringe   ; fringe tildes to mark beyond EOB
           (window-select +numbers)     ; visually switch windows
           (workspaces +tabs)        ; tab emulation, persistence & separate workspaces
           *basic (tab-bar +bufferlo)
           zen               ; distraction-free coding or writing

           :editor
           *basic (evil +everywhere); come to the dark side, we have cookies
           *basic file-templates    ; auto-snippets for empty files
           *basic fold              ; (nigh) universal code folding
           *basic format  ; automated prettiness
           god               ; run Emacs commands without modifier keys
           *extra license-headers
           *extra mindstream
           *basic multiple-cursors  ; editing in many places at once
           objed             ; text object editing for the innocent
           lispy             ; vim for lisp, for people who don't like vim
           *native parinfer          ; turn lisp into python, sort of
           *basic rotate-text       ; cycle region at point between text candidates
           *extra smart-tabs
           *basic snippets          ; my elves. They type so I don't have to
           *basic tempel ; replacement for :editor snippets
           *extra symex
           things
           *extra titlecase
           *extra typing-the-word-blimpy-in-doom-emacs
           *basic (whitespace +guess +trim)  ; a butler for your whitespace
           *basic word-wrap         ; soft wrapping with language-aware indent
           *basic words

           :emacs
           *basic bookmark-in-project
           *extra calc
           casual
           *basic (dired +icons +dirvish)             ; making dired pretty [functional]
           *basic electric          ; smarter, keyword-based electric-indent
           *basic electric-operator
           *extra elmacro
           *extra eww               ; the internet is gross
           *basic (ibuffer +icons)         ; interactive buffer management
           *extra keyfreq
           *extra perf-extras
           *basic repeat
           *basic secondary-selection
           *basic tramp             ; remote files at your arthritic fingertips
           *basic (:if (memq '*extra akn/tags) (tramp-extra +hlo +rpc) tramp-extra)
           *basic undo              ; persistent, smarter undo for your inevitable mistakes
           *basic vc                ; version-control and Emacs, sitting in a tree
           *basic vlf ; very large files

           :term
           *basic eshell            ; the elisp shell that works everywhere
           *basic eshell-extra
           *basic shell             ; simple shell REPL for Emacs
           *basic term              ; basic terminal emulator for Emacs
           *basic vterm             ; the best terminal emulation in Emacs
           *basic mistty
           *basic eat

           :checkers
           *basic syntax              ; tasing you for every semicolon you forget
           *extra (spell +enchant) ;+flyspell ; tasing you for misspelling mispelling
           grammar           ; tasing grammar mistake every you make

           :tools
           *extra age
           ansible
           biblio            ; Writes a PhD for you (citation needed)
           collab            ; buffers with friends
           *extra debugger          ; FIXME stepping through code, to help you add bugs
           *extra direnv
           *extra docker
           ;; *basic dwim-shell-command
           *basic editorconfig      ; let someone else argue about tabs vs spaces
           *basic (:if (versionp! emacs-version >= "30.1") vim-file-locals)
           ein               ; tame Jupyter notebooks with emacs
           *extra quarto
           *basic (:if (featurep :system 'linux) guix)
           *basic (eval +overlay)     ; run code, run (also, repls)
           *basic fasd
           *basic lookup              ; navigate your code and its documentation
           *extra (:if (executable-find "emacs-lsp-booster")
                       (lsp +eglot +booster)
                     (lsp +eglot))
           *basic (magit +forge)             ; a git porcelain for Emacs
           *extra (mason) ;+sync)
           *extra stgit
           *extra llm
           *extra llm-extra
           *basic make              ; run make tasks from Emacs
           *basic mise
           pass              ; password manager for nerds
           *extra pdf               ; pdf enhancements   ; NOTE: needed to do `brew install poppler`
           *basic prodigy           ; FIXME managing external services & code builders
           *basic regex
           terraform         ; infrastructure as code
           tmux              ; an API for interacting with tmux
           *extra tree-sitter       ; syntax and parsing, sitting in a tree...
           ;; TODO: are there security issues with :tools upload?
           *extra upload            ; map local to remote projects via ssh/ftp

           :os
           *basic (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
           *basic (:if (and (featurep :system 'macos) (boundp 'mac-carbon-version-string)) (emacs-mac +fullscreen))
           *basic (:if (and (featurep :system 'macos) (not (boundp 'mac-carbon-version-string))) emacs-plus)
           *basic tty               ; improve the terminal Emacs experience
           *basic (:if (featurep :system 'linux) exwm)

           :lang
           agda              ; types of types of types of types...
           *extra (:if (featurep :system 'macos) applescript)
           *extra beancount         ; mind the GAAP
           *extra ledger            ; be audit you can be
           *extra ledger+hledger
           *basic (cc +lsp)         ; C > C++ == 1
           *extra clojure           ; java with a lisp
           *extra common-lisp       ; if you've seen one lisp, you've seen them all
           *extra coq               ; proofs-as-programs
           *extra crystal           ; ruby at the speed of c
           *extra csharp            ; unity, .NET, and mono shenanigans
           *basic data              ; config/data formats
           *basic vimrc
           (dart +flutter)   ; paint ui and not much else
           *extra dhall
           *extra (elixir +tree-sitter)            ; erlang done right
           *extra (elm +lsp)               ; care for a cup of TEA?
           *basic emacs-lisp        ; drown in parentheses
           erlang            ; an elegant language for a more civilized age
           *extra (ess)               ; emacs speaks statistics (R) ; seems to work better without +tree-sitter
           factor
           faust             ; dsp, but you get to keep your soul
           *extra (fortran +lsp)           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
           *extra fsharp            ; ML stands for Microsoft's Language
           fstar             ; (dependent) types and (monadic) effects and Z3
           gdscript          ; the language you waited for
           (go +lsp)         ; the hipster dialect
           *extra (graphql +lsp)    ; Give queries a REST
           *extra (haskell +lsp)    ; a language that's lazier than I am
           *extra hare
           *extra hy                ; readability of scheme w/ speed of python
           *extra idris             ; a language you can depend on
           *extra (json +lsp) ;+tree-sitter             ; At least it ain't XML
           *basic jsonian
           janet             ; Fun fact: Janet is me!
           *extra (java +lsp)       ; the poster child for carpal tunnel syndrome
           *extra (javascript +lsp +tree-sitter)        ; all(hope(abandon(ye(who(enter(here))))))
           *extra julia             ; a better, faster MATLAB
           *extra kotlin            ; a better, slicker Java(Script)
           *extra (latex +fold +latexmk +fontification +viewers)             ; writing papers in Emacs has never been so fun
           *extra lean              ; for folks with too much to prove
           *basic (lua +fennel)               ; one-based indices? one-based indices
           *basic markdown          ; writing docs for people to ignore
           *extra mediawiki ; Wikipedia
           *extra obsidian
           *extra mercury
           nim               ; python + lisp at the speed of c
           *basic (nix +tree-sitter)               ; I hereby declare "nix geht mehr!"
           *extra (ocaml +tree-sitter)             ; an objective camel
           *extra (org +pretty +noter +dragndrop +pandoc)               ; organize your plain life in plain text (NOTE: disable +pretty if org gets slow)
           php               ; perl's insecure younger brother
           plantuml          ; diagrams for confusing people more
           *extra graphviz          ; diagrams for confusing yourself even more
           *extra pdf-raw
           purescript        ; javascript, but functional
           *basic (python +lsp +tree-sitter +pyright)            ; beautiful is better than ugly
           qt                ; the 'cutest' gui framework ever
           *extra (racket +lsp +xp)            ; a DSL for DSLs
           *extra raku              ; the artist formerly known as perl6
           *basic (rest +jq)              ; Emacs as a REST client
           *extra (roc +lsp)
           *basic rst               ; ReST in peace
           *extra (ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
           *extra (rust +lsp +tree-sitter)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
           scala             ; java, but good
           *extra (scheme +guile)   ; a fully conniving family of lisps
           *basic (sh +fish +powershell +lsp)        ; +tree-sitter ; she sells {ba,z,fi}sh shells on the C xor
           sml
           solidity          ; do you need a blockchain? No.
           *extra swift             ; who asked for emoji variables?
           terra             ; Earth and Moon in alignment for performance.
           *extra (typst +lsp)
           *extra (web +lsp +tree-sitter)               ; the tubes
           *extra (vue)
           *basic (yaml) ; +tree-sitter              ; JSON, but readable
           *extra (zig +lsp +tree-sitter)               ; C, but simpler
           *extra sage
           *extra maxima
           *extra linear-program
           uiua
           *basic log
           *extra kdl

           :email
           (mu4e +org +gmail)
           *extra (notmuch +org)
           (wanderlust +gmail)

           :app
           calendar    ; Watch your missed deadlines in real time (with org and google calendar sync support)
           *extra omni
           emms        ; A media player for music no one's heard of
           *extra everywhere        ; *leave* Emacs!? You must be joking
           *extra github-explorer
           irc               ; how neckbeards socialize
           *extra rss        ; emacs as an RSS reader
           *extra transcribe
           twitter           ; twitter client https://twitter.com/vnought
           *extra xkcd

           :config
           literate
           *basic (default +bindings +smartparens +gnupg)
           *basic (akn-bindings +leader-keys +leader-bindings +evil-insert))
