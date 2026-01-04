(import-macros {: tx} :config.macros)

[;; Tabline
 (tx "nvim-mini/mini.tabline" {:version "*" :opts {}})

 ;; Statusline
 (tx "nvim-mini/mini.statusline"
     {:version "*"
      :opts {:use_icons vim.g.have_nerd_font}})

 ;; (tx "nvim-mini/mini.animate"
 ;;     {:version "*"
 ;;      :event :VeryLazy
 ;;      :cond (= vim.g.neovide nil)
 ;;      :opts {:cursor {:enable false}
 ;;             :scroll {:enable false}
 ;;             :resize {:enable true}
 ;;             :open   {:enable true}
 ;;             :close  {:enable true}}})

 ;; Smooth scrolling
 (tx "karb94/neoscroll.nvim"
     {:cond (= vim.g.neovide nil)
      :event :VeryLazy
      :opts {:hide_cursor true
             :stop_eof true
             :respect_scrolloff false
             :cursor_scrolls_alone true
             :duration_multiplier 0.3
             :easing :linear
             :pre_hook nil
             :post_hook nil
             :performance_mode true
             :ignored_events [:WinScrolled :CursorMoved]}})]
