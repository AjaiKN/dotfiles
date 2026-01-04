-- [nfnl] fnl/plugins/lsp.fnl
local function _1_()
  local function _2_(event)
    local map
    local function _3_(keys, func, desc, mode)
      local mode0 = (mode or "n")
      return vim.keymap.set(mode0, keys, func, {buffer = event.buf, desc = ("LSP: " .. desc)})
    end
    map = _3_
    map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
    map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
    map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
    map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")
    map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
    map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
    map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
    map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction", {"n", "x"})
    map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
    local client_supports_method
    local function _4_(client, method, bufnr)
      if (vim.fn.has("nvim-0.11") == 1) then
        return client:supports_method(method, bufnr)
      else
        return client.supports_method(method, {bufnr = bufnr})
      end
    end
    client_supports_method = _4_
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    if (client and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf)) then
      local highlight_augroup = vim.api.nvim_create_augroup("kickstart-lsp-highlight", {clear = false})
      vim.api.nvim_create_autocmd({"CursorHold", "CursorHoldI"}, {buffer = event.buf, group = highlight_augroup, callback = vim.lsp.buf.document_highlight})
      vim.api.nvim_create_autocmd({"CursorMoved", "CursorMovedI"}, {buffer = event.buf, group = highlight_augroup, callback = vim.lsp.buf.clear_references})
      local function _6_(event2)
        vim.lsp.buf.clear_references()
        return vim.api.nvim_clear_autocmds({group = "kickstart-lsp-highlight", buffer = event2.buf})
      end
      vim.api.nvim_create_autocmd("LspDetach", {group = vim.api.nvim_create_augroup("kickstart-lsp-detach", {clear = true}), callback = _6_})
    else
    end
    if (client and client_supports_method(client, vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf)) then
      local function _8_()
        return vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({bufnr = event.buf}))
      end
      return map("<leader>th", _8_, "[T]oggle Inlay [H]ints")
    else
      return nil
    end
  end
  vim.api.nvim_create_autocmd("LspAttach", {group = vim.api.nvim_create_augroup("kickstart-lsp-attach", {clear = true}), callback = _2_})
  local _10_
  if vim.g.have_nerd_font then
    _10_ = {text = {[vim.diagnostic.severity.ERROR] = "\243\176\133\154 ", [vim.diagnostic.severity.WARN] = "\243\176\128\170 ", [vim.diagnostic.severity.INFO] = "\243\176\139\189 ", [vim.diagnostic.severity.HINT] = "\243\176\140\182 "}}
  else
    _10_ = {}
  end
  local function _12_(diagnostic)
    return ({[vim.diagnostic.severity.ERROR] = diagnostic.message, [vim.diagnostic.severity.WARN] = diagnostic.message, [vim.diagnostic.severity.INFO] = diagnostic.message, [vim.diagnostic.severity.HINT] = diagnostic.message})[diagnostic.severity]
  end
  vim.diagnostic.config({severity_sort = true, float = {border = "rounded", source = "if_many"}, underline = {severity = vim.diagnostic.severity.ERROR}, signs = _10_, virtual_text = {source = "if_many", spacing = 2, format = _12_}})
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  local capabilities0 = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())
  local servers = {rust_analyzer = {}, lua_ls = {settings = {Lua = {completion = {callSnippet = "Replace"}}}}}
  local ensure_installed = vim.tbl_keys((servers or {}))
  vim.list_extend(ensure_installed, {"stylua"})
  require("mason-tool-installer").setup({ensure_installed = ensure_installed})
  local function _13_(server_name)
    local server = (servers[server_name] or {})
    server.capabilities = vim.tbl_deep_extend("force", {}, capabilities0, (server.capabilities or {}))
    return require("lspconfig")[server_name].setup(server)
  end
  return require("mason-lspconfig").setup({ensure_installed = {}, handlers = {_13_}, automatic_installation = false})
end
return {{"folke/lazydev.nvim", ft = "lua", opts = {library = {{path = "${3rd}/luv/library", words = {"vim%.uv"}}}}}, {"neovim/nvim-lspconfig", config = _1_, dependencies = {{"williamboman/mason.nvim", opts = {}}, "williamboman/mason-lspconfig.nvim", "WhoIsSethDaniel/mason-tool-installer.nvim", {"j-hui/fidget.nvim", opts = {}}, "hrsh7th/cmp-nvim-lsp"}}}
