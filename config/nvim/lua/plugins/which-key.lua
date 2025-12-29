-- [nfnl] fnl/plugins/which-key.fnl
local _1_
if vim.g.have_nerd_font then
  _1_ = {}
else
  _1_ = {Up = "<Up> ", Down = "<Down> ", Left = "<Left> ", Right = "<Right> ", C = "<C-\226\128\166> ", M = "<M-\226\128\166> ", D = "<D-\226\128\166> ", S = "<S-\226\128\166> ", CR = "<CR> ", Esc = "<Esc> ", ScrollWheelDown = "<ScrollWheelDown> ", ScrollWheelUp = "<ScrollWheelUp> ", NL = "<NL> ", BS = "<BS> ", Space = "<Space> ", Tab = "<Tab> ", F1 = "<F1>", F2 = "<F2>", F3 = "<F3>", F4 = "<F4>", F5 = "<F5>", F6 = "<F6>", F7 = "<F7>", F8 = "<F8>", F9 = "<F9>", F10 = "<F10>", F11 = "<F11>", F12 = "<F12>"}
end
return {"folke/which-key.nvim", event = "VimEnter", opts = {delay = 0, icons = {mappings = vim.g.have_nerd_font, keys = _1_}, spec = {{"<leader>c", group = "[C]ode", mode = {"n", "x"}}, {"<leader>d", group = "[D]ocument"}, {"<leader>r", group = "[R]ename"}, {"<leader>s", group = "[S]earch"}, {"<leader>w", group = "[W]orkspace"}, {"<leader>t", group = "[T]oggle"}, {"<leader>h", group = "Git [H]unk", mode = {"n", "v"}}}}}
