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
      :keys [(tx :s {:mode [:n :x :o] 1 (fn [] ((. (require :flash) :jump))) :desc "Flash"})
             (tx :S {:mode [:n :x :o] 1 (fn [] ((. (require :flash) :treesitter))) :desc "Flash Treesitter"})
             (tx :r {:mode :o 1 (fn [] ((. (require :flash) :remote))) :desc "Remote Flash"})
             (tx :R {:mode [:o :x] 1 (fn [] ((. (require :flash) :treesitter_search))) :desc "Treesitter Search"})
             (tx :<c-s> {:mode [:c] 1 (fn [] ((. (require :flash) :toggle))) :desc "Toggle Flash Search"})]})

 ;; Better Yank/Paste
 (tx "gbprod/yanky.nvim"
     {:desc "Better Yank/Paste"
      :event :VeryLazy
      :dependencies [(tx "kkharji/sqlite.lua")]
      :opts {:ring {:storage :sqlite}
             :highlight {:timer 150}}
      :keys [(tx :<leader>p {:mode [:n :x] 1 "<cmd>YankyRingHistory<cr>" :desc "Open Yank History"})
             (tx :y {:mode [:n :x] 1 "<Plug>(YankyYank)" :desc "Yank text"})
             (tx :p {:mode [:n :x] 1 "<Plug>(YankyPutAfter)" :desc "Put yanked text after cursor"})
             (tx :P {:mode [:n :x] 1 "<Plug>(YankyPutBefore)" :desc "Put yanked text before cursor"})
             (tx :gp {:mode [:n :x] 1 "<Plug>(YankyGPutAfter)" :desc "Put yanked text after selection"})
             (tx :gP {:mode [:n :x] 1 "<Plug>(YankyGPutBefore)" :desc "Put yanked text before selection"})
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
