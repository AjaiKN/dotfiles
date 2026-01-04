(import-macros {: tx} :config.macros)

;; Highlight todo, notes, etc in comments
(tx "folke/todo-comments.nvim"
    {:event :VimEnter
     :dependencies ["nvim-lua/plenary.nvim"]
     :opts {:signs false}})
