(import-macros {: tx} :config.macros)

[(tx "easymotion/vim-easymotion") ;; TODO: keybindings
 ;; "tpope/vim-commentary"
 ;; https://github.com/smoka7/hop.nvim
 (tx "tommcdo/vim-lion")
 ;; "tpope/vim-unimpaired"
 ;; "liuchengxu/vim-which-key"
 ;; "tpope/vim-fugitive"

 ;; Go forward/backward with square brackets
 (tx "nvim-mini/mini.bracketed" {:version "*" :opts {}})
 ;; Comment lines
 (tx "nvim-mini/mini.comment" {:version "*" :opts {}})
 ;; Autopairs
 (tx "nvim-mini/mini.pairs" {:version "*" :opts {}})
 ;; Trailspace (highlight and remove)
 (tx "nvim-mini/mini.trailspace" {:version "*" :opts {}})

 ;; Better Around/Inside textobjects
 ;;
 ;; Examples:
 ;;  - va)  - [V]isually select [A]round [)]paren
 ;;  - yinq - [Y]ank [I]nside [N]ext [Q]uote
 ;;  - ci'  - [C]hange [I]nside [']quote
 (tx "nvim-mini/mini.ai" {:version "*" :opts {:n_lines 500}})

 ;; Add/delete/replace surroundings (brackets, quotes, etc.)
 ;;
 ;; - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
 ;; - sd'   - [S]urround [D]elete [']quotes
 ;; - sr)'  - [S]urround [R]eplace [)] [']
 ;; (tx "nvim-mini/mini.surround" {:version "*" :opts {}})

 ;; "tpope/vim-surround"
 (tx "kylechui/nvim-surround" {:event :VeryLazy :opts {}})

 ;; "justinmk/vim-sneak"

 ;; Flash for quick jumping
 (tx "folke/flash.nvim"
     {:event :VeryLazy
      :opts {}
      :keys [(tx :s     (fn [] ((. (require :flash) :jump))) {:mode [:n :x :o] :desc "Flash"})
             (tx :S     (fn [] ((. (require :flash) :treesitter))) {:mode [:n :x :o] :desc "Flash Treesitter"})
             (tx :r     (fn [] ((. (require :flash) :remote))) {:mode :o :desc "Remote Flash"})
             (tx :R     (fn [] ((. (require :flash) :treesitter_search))) {:mode [:o :x] :desc "Treesitter Search"})
             (tx :<c-s> (fn [] ((. (require :flash) :toggle))) {:mode [:c] :desc "Toggle Flash Search"})]})

 ;; Better Yank/Paste
 (tx "gbprod/yanky.nvim"
     {:desc "Better Yank/Paste"
      :event :VeryLazy
      :dependencies [(tx "kkharji/sqlite.lua")]
      :opts {:ring {:storage :sqlite}
             :highlight {:timer 150}}
      :keys [(tx :<leader>p "<cmd>YankyRingHistory<cr>" {:mode [:n :x] :desc "Open Yank History"})
             (tx :y "<Plug>(YankyYank)" {:mode [:n :x] :desc "Yank text"})
             (tx :p "<Plug>(YankyPutAfter)" {:mode [:n :x] :desc "Put yanked text after cursor"})
             (tx :P "<Plug>(YankyPutBefore)" {:mode [:n :x] :desc "Put yanked text before cursor"})
             (tx :gp "<Plug>(YankyGPutAfter)" {:mode [:n :x] :desc "Put yanked text after selection"})
             (tx :gP "<Plug>(YankyGPutBefore)" {:mode [:n :x] :desc "Put yanked text before selection"})
             (tx :<M-y> "<Plug>(YankyCycleForward)" {:desc "Cycle Forward Through Yank History"})
             (tx "[y" "<Plug>(YankyCycleForward)" {:desc "Cycle Forward Through Yank History"})
             (tx "]y" "<Plug>(YankyCycleBackward)" {:desc "Cycle Backward Through Yank History"})
             (tx :<c-p> "<Plug>(YankyPreviousEntry)" {:desc "Select previous entry through yank history"})
             (tx :<c-n> "<Plug>(YankyNextEntry)" {:desc "Select next entry through yank history"})
             (tx "]p" "<Plug>(YankyPutIndentAfterLinewise)" {:desc "Put indented after cursor (linewise)"})
             (tx "]P" "<Plug>(YankyPutIndentAfterLinewise)" {:desc "Put indented after cursor (linewise)"})
             (tx "[p" "<Plug>(YankyPutIndentBeforeLinewise)" {:desc "Put indented before cursor (linewise)"})
             (tx "[P" "<Plug>(YankyPutIndentBeforeLinewise)" {:desc "Put indented before cursor (linewise)"})
             (tx ">p" "<Plug>(YankyPutIndentAfterShiftRight)" {:desc "Put and indent right"})
             (tx "<p" "<Plug>(YankyPutIndentAfterShiftLeft)" {:desc "Put and indent left"})
             (tx ">P" "<Plug>(YankyPutIndentBeforeShiftRight)" {:desc "Put before and indent right"})
             (tx "<P" "<Plug>(YankyPutIndentBeforeShiftLeft)" {:desc "Put before and indent left"})
             (tx "=p" "<Plug>(YankyPutAfterFilter)" {:desc "Put after applying a filter"})
             (tx "=P" "<Plug>(YankyPutBeforeFilter)" {:desc "Put before applying a filter"})]})]
