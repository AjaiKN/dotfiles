(import-macros {: tx} :config.macros)

[(tx "rhysd/git-messenger.vim")
 (tx "tpope/vim-sleuth")

 ;; Git integration, similar to vim-fugitive
 (tx "nvim-mini/mini-git" {:name "mini.git" :version "*" :opts {}})

 ;; Adds git related signs to the gutter, as well as utilities for managing changes
 (tx "lewis6991/gitsigns.nvim"
     {:opts {:signs {:add {:text "+"}
                     :change {:text "~"}
                     :delete {:text "_"}
                     :topdelete {:text "‾"}
                     :changedelete {:text "~"}}}})]
