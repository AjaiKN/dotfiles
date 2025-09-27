return {
  -- 'tpope/vim-commentary',
  'easymotion/vim-easymotion', -- TODO: keybindings
  -- https://github.com/smoka7/hop.nvim
  'tommcdo/vim-lion',
  -- 'tpope/vim-unimpaired',
  -- 'liuchengxu/vim-which-key',
  -- 'tpope/vim-fugitive',

  -- Go forward/backward with square brackets
  { 'nvim-mini/mini.bracketed', version = '*', opts = {} },
  -- Comment lines
  { 'nvim-mini/mini.comment', version = '*', opts = {} },
  -- Autopairs
  { 'nvim-mini/mini.pairs', version = '*', opts = {} },
  -- Trailspace (highlight and remove)
  { 'nvim-mini/mini.trailspace', version = '*', opts = {} },

  -- Better Around/Inside textobjects
  --
  -- Examples:
  --  - va)  - [V]isually select [A]round [)]paren
  --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
  --  - ci'  - [C]hange [I]nside [']quote
  {
    'nvim-mini/mini.ai',
    version = '*',
    opts = {
      n_lines = 500
    },
  },

  -- Add/delete/replace surroundings (brackets, quotes, etc.)
  --
  -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
  -- - sd'   - [S]urround [D]elete [']quotes
  -- - sr)'  - [S]urround [R]eplace [)] [']
  -- { 'nvim-mini/mini.surround', version = '*', opts = {} },

  -- 'tpope/vim-surround',
  {
    'kylechui/nvim-surround',
    event = 'VeryLazy',
    opts = {},
  },

  -- 'justinmk/vim-sneak',
  {
    'folke/flash.nvim',
    event = 'VeryLazy',
    ---@type Flash.Config
    opts = {},
    -- stylua: ignore
    keys = {
      { 's', mode = { 'n', 'x', 'o' }, function() require('flash').jump() end, desc = 'Flash' },
      { 'S', mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end, desc = 'Flash Treesitter' },
      { 'r', mode = 'o', function() require('flash').remote() end, desc = 'Remote Flash' },
      { 'R', mode = { 'o', 'x' }, function() require('flash').treesitter_search() end, desc = 'Treesitter Search' },
      { '<c-s>', mode = { 'c' }, function() require('flash').toggle() end, desc = 'Toggle Flash Search' },
    },
  },

  {
    'karb94/neoscroll.nvim',
    opts = {
      hide_cursor = true,           -- Hide cursor while scrolling
      stop_eof = true,              -- Stop at <EOF> when scrolling downwards
      respect_scrolloff = false,    -- Stop scrolling when the cursor reaches the scrolloff margin of the file
      cursor_scrolls_alone = false, -- The cursor will keep on scrolling even if the window cannot scroll further
      duration_multiplier = 0.5,    -- Global duration multiplier
      easing = 'linear',            -- Default easing function (linear, quadratic, cubic, quartic, quintic, circular, sine)
      pre_hook = nil,               -- Function to run before the scrolling animation starts
      post_hook = nil,              -- Function to run after the scrolling animation ends
      performance_mode = true,      -- Disable "Performance Mode" on all buffers (turns off syntax highlighting while scrolling)
      ignored_events = {            -- Events ignored while scrolling
        'WinScrolled', 'CursorMoved'
      },
    },
  },

  {
    'gbprod/yanky.nvim',
    desc = 'Better Yank/Paste',
    event = 'VeryLazy',
    dependencies = {
      { 'kkharji/sqlite.lua' }
    },
    opts = {
      ring = { storage = 'sqlite' },
      highlight = { timer = 150 },
    },
    keys = {
      { '<leader>p', '<cmd>YankyRingHistory<cr>', mode = { 'n', 'x' }, desc = 'Open Yank History' },
      { 'y', '<Plug>(YankyYank)', mode = { 'n', 'x' }, desc = 'Yank text' },
      { 'p', '<Plug>(YankyPutAfter)', mode = { 'n', 'x' }, desc = 'Put yanked text after cursor' },
      { 'P', '<Plug>(YankyPutBefore)', mode = { 'n', 'x' }, desc = 'Put yanked text before cursor' },
      { 'gp', '<Plug>(YankyGPutAfter)', mode = { 'n', 'x' }, desc = 'Put yanked text after selection' },
      { 'gP', '<Plug>(YankyGPutBefore)', mode = { 'n', 'x' }, desc = 'Put yanked text before selection' },

      { '<M-y>', '<Plug>(YankyCycleForward)', desc = 'Cycle Forward Through Yank History' },
      { '[y', '<Plug>(YankyCycleForward)', desc = 'Cycle Forward Through Yank History' },
      { ']y', '<Plug>(YankyCycleBackward)', desc = 'Cycle Backward Through Yank History' },

      { '<c-p>', '<Plug>(YankyPreviousEntry)', desc = 'Select previous entry through yank history' },
      { '<c-n>', '<Plug>(YankyNextEntry)', desc = 'Select next entry through yank history' },
      { ']p', '<Plug>(YankyPutIndentAfterLinewise)', desc = 'Put indented after cursor (linewise)' },
      { ']P', '<Plug>(YankyPutIndentAfterLinewise)', desc = 'Put indented after cursor (linewise)' },
      { '[p', '<Plug>(YankyPutIndentBeforeLinewise)', desc = 'Put indented before cursor (linewise)' },
      { '[P', '<Plug>(YankyPutIndentBeforeLinewise)', desc = 'Put indented before cursor (linewise)' },
      { '>p', '<Plug>(YankyPutIndentAfterShiftRight)', desc = 'Put and indent right' },
      { '<p', '<Plug>(YankyPutIndentAfterShiftLeft)', desc = 'Put and indent left' },
      { '>P', '<Plug>(YankyPutIndentBeforeShiftRight)', desc = 'Put before and indent right' },
      { '<P', '<Plug>(YankyPutIndentBeforeShiftLeft)', desc = 'Put before and indent left' },
      { '=p', '<Plug>(YankyPutAfterFilter)', desc = 'Put after applying a filter' },
      { '=P', '<Plug>(YankyPutBeforeFilter)', desc = 'Put before applying a filter' },
    },
  }
}
