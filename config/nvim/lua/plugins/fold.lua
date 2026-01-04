-- [nfnl] fnl/plugins/fold.fnl
local function _1_(_, opts)
  vim.o.foldcolumn = "1"
  vim.o.foldlevel = 99
  vim.o.foldlevelstart = 99
  vim.o.foldenable = true
  require("ufo").setup(opts)
  vim.keymap.set("n", "zR", require("ufo").openAllFolds)
  vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
  return vim.keymap.set("n", "<Tab>", "za")
end
local function _2_(bufnr, filetype, buftype)
  return {"treesitter", "indent"}
end
return {"kevinhwang91/nvim-ufo", config = _1_, dependencies = {"kevinhwang91/promise-async"}, opts = {provider_selector = _2_}}
