(import-macros {: tx} :config.macros)

;; Autocompletion
(tx "hrsh7th/nvim-cmp"
    {:event :InsertEnter
     :dependencies
     [;; Snippet Engine & its associated nvim-cmp source
      (tx "L3MON4D3/LuaSnip"
          {;; :dependencies
           ;; [ ;; `friendly-snippets` contains a variety of premade snippets.
           ;;   ;;    See the README about individual language/framework/plugin snippets:
           ;;   ;;    https://github.com/rafamadriz/friendly-snippets
           ;;   (tx "rafamadriz/friendly-snippets"
           ;;       {:config (fn [] ((. (require :luasnip.loaders.from_vscode) :lazy_load)))})]
           :build (let [has-win (= (vim.fn.has :win32) 1)
                        has-make (= (vim.fn.executable :make) 1)]
                    (when (and (not has-win) has-make)
                      "make install_jsregexp"))})
      "saadparwaiz1/cmp_luasnip"
      ;; Adds other completion capabilities.
      ;;  nvim-cmp does not ship with all sources by default. They are split
      ;;  into multiple repos for maintenance purposes.
      "hrsh7th/cmp-nvim-lsp"
      "hrsh7th/cmp-path"
      "hrsh7th/cmp-nvim-lsp-signature-help"]
     :config
     (fn []
       ;; See `:help cmp`
       (local cmp (require :cmp))
       (local luasnip (require :luasnip))
       (luasnip.config.setup {})

       (cmp.setup
         {:snippet {:expand (fn [args] (luasnip.lsp_expand args.body))}
          :completion {:completeopt "menu,menuone,noinsert"}

          ;; For an understanding of why these mappings were
          ;; chosen, you will need to read `:help ins-completion`
          ;;
          ;; No, but seriously. Please read `:help ins-completion`, it is really good!
          ;; For more advanced Luasnip keymaps (e.g. selecting choice nodes, expansion) see:
          ;;    https://github.com/L3MON4D3/LuaSnip?tab=readme-ov-file#keymaps
          :mapping (cmp.mapping.preset.insert
                     {;; Select the [n]ext item
                      :<C-n> (cmp.mapping.select_next_item)
                      ;; Select the [p]revious item
                      :<C-p> (cmp.mapping.select_prev_item)
                      ;; Scroll the documentation window [b]ack / [f]orward
                      :<C-b> (cmp.mapping.scroll_docs -4)
                      :<C-f> (cmp.mapping.scroll_docs 4)
                      ;; Accept ([y]es) the completion.
                      ;;  This will auto-import if your LSP supports it.
                      ;;  This will expand snippets if the LSP sent a snippet.
                      :<C-y> (cmp.mapping.confirm {:select true})
                      ;; If you prefer more traditional completion keymaps,
                      ;; you can uncomment the following lines
                      ;; :<CR> (cmp.mapping.confirm {:select true})
                      ;; :<Tab> (cmp.mapping.select_next_item)
                      ;; :<S-Tab> (cmp.mapping.select_prev_item)
                      ;; Manually trigger a completion from nvim-cmp.
                      ;;  Generally you don't need this, because nvim-cmp will display
                      ;;  completions whenever it has completion options available.
                      :<C-Space> (cmp.mapping.complete {})
                      ;; Think of <c-l> as moving to the right of your snippet expansion.
                      ;;  So if you have a snippet that's like:
                      ;;  function $name($args)
                      ;;    $body
                      ;;  end
                      ;;
                      ;; <c-l> will move you to the right of each of the expansion locations.
                      ;; <c-h> is similar, except moving you backwards.
                      :<C-l> (cmp.mapping (fn []
                                            (when (luasnip.expand_or_locally_jumpable)
                                              (luasnip.expand_or_jump)))
                                          [:i :s])
                      :<C-h> (cmp.mapping (fn []
                                            (when (luasnip.locally_jumpable -1)
                                              (luasnip.jump -1)))
                                          [:i :s])})
          :sources [{:name :lazydev
                       ;; set group index to 0 to skip loading LuaLS completions as lazydev recommends it
                       :group_index 0}
                    {:name :nvim_lsp}
                    {:name :luasnip}
                    {:name :path}
                    {:name :nvim_lsp_signature_help}]}))})
