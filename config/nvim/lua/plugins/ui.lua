return {
  -- Tabline
  {
    'nvim-mini/mini.tabline', version = '*',
    opts = {},
  },

  {
    'nvim-mini/mini.statusline', version = '*',
    opts = {
      use_icons = vim.g.have_nerd_font
    },
  },

  -- {
  --   'nvim-mini/mini.animate',
  --   version = '*',
  --   event = 'VeryLazy',
  --   cond = vim.g.neovide == nil,
  --   opts = {
  --     cursor = { enable = false },
  --     scroll = { enable = false },
  --     resize = { enable = true },
  --     open   = { enable = true },
  --     close  = { enable = true },
  --   },
  -- },

  {
    'karb94/neoscroll.nvim',
    cond = vim.g.neovide == nil,
    event = 'VeryLazy',
    opts = {
      hide_cursor = true,           -- Hide cursor while scrolling
      stop_eof = true,              -- Stop at <EOF> when scrolling downwards
      respect_scrolloff = false,    -- Stop scrolling when the cursor reaches the scrolloff margin of the file
      cursor_scrolls_alone = true,  -- The cursor will keep on scrolling even if the window cannot scroll further
      duration_multiplier = 0.3,    -- Global duration multiplier
      easing = 'linear',            -- Default easing function (linear, quadratic, cubic, quartic, quintic, circular, sine)
      pre_hook = nil,               -- Function to run before the scrolling animation starts
      post_hook = nil,              -- Function to run after the scrolling animation ends
      performance_mode = true,      -- Disable "Performance Mode" on all buffers (turns off syntax highlighting while scrolling)
      ignored_events = {            -- Events ignored while scrolling
        'WinScrolled', 'CursorMoved'
      },
    },
  },
}
