-- [nfnl] fnl/plugins/completion.fnl
local function _1_()
  local cmp = require("cmp")
  local luasnip = require("luasnip")
  luasnip.config.setup({})
  local function _2_(args)
    return luasnip.lsp_expand(args.body)
  end
  local function _3_()
    if luasnip.expand_or_locally_jumpable() then
      return luasnip.expand_or_jump()
    else
      return nil
    end
  end
  local function _5_()
    if luasnip.locally_jumpable(-1) then
      return luasnip.jump(-1)
    else
      return nil
    end
  end
  return cmp.setup({snippet = {expand = _2_}, completion = {completeopt = "menu,menuone,noinsert"}, mapping = cmp.mapping.preset.insert({["<C-n>"] = cmp.mapping.select_next_item(), ["<C-p>"] = cmp.mapping.select_prev_item(), ["<C-b>"] = cmp.mapping.scroll_docs(-4), ["<C-f>"] = cmp.mapping.scroll_docs(4), ["<C-y>"] = cmp.mapping.confirm({select = true}), ["<C-Space>"] = cmp.mapping.complete({}), ["<C-l>"] = cmp.mapping(_3_, {"i", "s"}), ["<C-h>"] = cmp.mapping(_5_, {"i", "s"})}), sources = {{name = "lazydev", group_index = 0}, {name = "nvim_lsp"}, {name = "luasnip"}, {name = "path"}, {name = "nvim_lsp_signature_help"}}})
end
local _7_
do
  local has_win = (vim.fn.has("win32") == 1)
  local has_make = (vim.fn.executable("make") == 1)
  if (not has_win and has_make) then
    _7_ = "make install_jsregexp"
  else
    _7_ = nil
  end
end
return {"hrsh7th/nvim-cmp", config = _1_, dependencies = {{"L3MON4D3/LuaSnip", build = _7_}, "saadparwaiz1/cmp_luasnip", "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-path", "hrsh7th/cmp-nvim-lsp-signature-help"}, event = "InsertEnter"}
