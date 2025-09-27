return {
  'rhysd/git-messenger.vim',
  'tpope/vim-sleuth',

  -- Git integration, similar to vim-fugitive
  { 'nvim-mini/mini-git', name = 'mini.git', version = '*', opts = {} },


  { -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
  },
}
