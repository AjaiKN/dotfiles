-- [nfnl] fnl/plugins/editing.fnl
local function _1_()
  return require("flash").jump()
end
local function _2_()
  return require("flash").treesitter()
end
local function _3_()
  return require("flash").remote()
end
local function _4_()
  return require("flash").treesitter_search()
end
local function _5_()
  return require("flash").toggle()
end
return {{"easymotion/vim-easymotion"}, {"tommcdo/vim-lion"}, {"nvim-mini/mini.bracketed", opts = {}, version = "*"}, {"nvim-mini/mini.comment", opts = {}, version = "*"}, {"nvim-mini/mini.pairs", opts = {}, version = "*"}, {"nvim-mini/mini.trailspace", opts = {}, version = "*"}, {"nvim-mini/mini.ai", opts = {n_lines = 500}, version = "*"}, {"kylechui/nvim-surround", event = "VeryLazy", opts = {}}, {"folke/flash.nvim", event = "VeryLazy", keys = {{"s", _1_, desc = "Flash", mode = {"n", "x", "o"}}, {"S", _2_, desc = "Flash Treesitter", mode = {"n", "x", "o"}}, {"r", _3_, desc = "Remote Flash", mode = "o"}, {"R", _4_, desc = "Treesitter Search", mode = {"o", "x"}}, {"<c-s>", _5_, desc = "Toggle Flash Search", mode = {"c"}}}, opts = {}}, {"gbprod/yanky.nvim", dependencies = {{"kkharji/sqlite.lua"}}, desc = "Better Yank/Paste", event = "VeryLazy", keys = {{"<leader>p", "<cmd>YankyRingHistory<cr>", desc = "Open Yank History", mode = {"n", "x"}}, {"y", "<Plug>(YankyYank)", desc = "Yank text", mode = {"n", "x"}}, {"p", "<Plug>(YankyPutAfter)", desc = "Put yanked text after cursor", mode = {"n", "x"}}, {"P", "<Plug>(YankyPutBefore)", desc = "Put yanked text before cursor", mode = {"n", "x"}}, {"gp", "<Plug>(YankyGPutAfter)", desc = "Put yanked text after selection", mode = {"n", "x"}}, {"gP", "<Plug>(YankyGPutBefore)", desc = "Put yanked text before selection", mode = {"n", "x"}}, {"<M-y>", "<Plug>(YankyCycleForward)", desc = "Cycle Forward Through Yank History"}, {"[y", "<Plug>(YankyCycleForward)", desc = "Cycle Forward Through Yank History"}, {"]y", "<Plug>(YankyCycleBackward)", desc = "Cycle Backward Through Yank History"}, {"<c-p>", "<Plug>(YankyPreviousEntry)", desc = "Select previous entry through yank history"}, {"<c-n>", "<Plug>(YankyNextEntry)", desc = "Select next entry through yank history"}, {"]p", "<Plug>(YankyPutIndentAfterLinewise)", desc = "Put indented after cursor (linewise)"}, {"]P", "<Plug>(YankyPutIndentAfterLinewise)", desc = "Put indented after cursor (linewise)"}, {"[p", "<Plug>(YankyPutIndentBeforeLinewise)", desc = "Put indented before cursor (linewise)"}, {"[P", "<Plug>(YankyPutIndentBeforeLinewise)", desc = "Put indented before cursor (linewise)"}, {">p", "<Plug>(YankyPutIndentAfterShiftRight)", desc = "Put and indent right"}, {"<p", "<Plug>(YankyPutIndentAfterShiftLeft)", desc = "Put and indent left"}, {">P", "<Plug>(YankyPutIndentBeforeShiftRight)", desc = "Put before and indent right"}, {"<P", "<Plug>(YankyPutIndentBeforeShiftLeft)", desc = "Put before and indent left"}, {"=p", "<Plug>(YankyPutAfterFilter)", desc = "Put after applying a filter"}, {"=P", "<Plug>(YankyPutBeforeFilter)", desc = "Put before applying a filter"}}, opts = {ring = {storage = "sqlite"}, highlight = {timer = 150}}}}
