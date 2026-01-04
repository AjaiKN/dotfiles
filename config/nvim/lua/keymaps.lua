-- [nfnl] fnl/keymaps.fnl
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, {desc = "Open diagnostic [Q]uickfix list"})
local function _1_()
  return vim.highlight.on_yank()
end
return vim.api.nvim_create_autocmd("TextYankPost", {desc = "Highlight when yanking (copying) text", group = vim.api.nvim_create_augroup("kickstart-highlight-yank", {clear = true}), callback = _1_})
