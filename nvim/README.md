# mello-nvim

A Neovim plugin for mello

To use this, add it to your `lazy.nvim` plugins:

    {
      dir = 'mello/nvim',
      lazy = true,
      ft = { 'mello' },
      dependencies = {
        'nvim-treesitter/nvim-treesitter',
      },
      init = function()
        vim.filetype.add { extension = { mello = 'mlo' } }
      end,
    },

