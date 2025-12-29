(import-macros {: tx} :config.macros)

;; Useful plugin to show you pending keybinds.
(tx "folke/which-key.nvim"
    {:event "VimEnter"
     :opts {:delay 0 ;(independent of vim.opt.timeoutlen)
            :icons {:mappings vim.g.have_nerd_font
                    ;; If you are using a Nerd Font: set icons.keys to an empty
                    ;; table which will use the default which-key.nvim defined
                    ;; Nerd Font icons
                    :keys (if vim.g.have_nerd_font {}
                              {:Up "<Up> "
                               :Down "<Down> "
                               :Left "<Left> "
                               :Right "<Right> "
                               :C "<C-…> "
                               :M "<M-…> "
                               :D "<D-…> "
                               :S "<S-…> "
                               :CR "<CR> "
                               :Esc "<Esc> "
                               :ScrollWheelDown "<ScrollWheelDown> "
                               :ScrollWheelUp "<ScrollWheelUp> "
                               :NL "<NL> "
                               :BS "<BS> "
                               :Space "<Space> "
                               :Tab "<Tab> "
                               :F1 "<F1>"
                               :F2 "<F2>"
                               :F3 "<F3>"
                               :F4 "<F4>"
                               :F5 "<F5>"
                               :F6 "<F6>"
                               :F7 "<F7>"
                               :F8 "<F8>"
                               :F9 "<F9>"
                               :F10 "<F10>"
                               :F11 "<F11>"
                               :F12 "<F12>"})}
            ;; -- Document existing key chains
            :spec [(tx "<leader>c" {:group "[C]ode" :mode [:n :x]})
                   (tx "<leader>d" {:group "[D]ocument"})
                   (tx "<leader>r" {:group "[R]ename"})
                   (tx "<leader>s" {:group "[S]earch"})
                   (tx "<leader>w" {:group "[W]orkspace"})
                   (tx "<leader>t" {:group "[T]oggle"})
                   (tx "<leader>h" {:group "Git [H]unk" :mode [:n :v]})]}})
