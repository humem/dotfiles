return {
  -- netrw

  lsp = {
    servers = {
      "pyright",
    },
  },

  mappings = {
    n = {
      ["<space>"] = { "<pagedown>", desc = "Scroll down" },
      ["<S-space>"] = { "<pageup>", desc = "Scroll up" },
      ["<leader>Q"] = { "<cmd>qa<cr>", desc = "Quit all" },
      ["<leader>r"] = { "<cmd>e<cr>", desc = "Reload" },
      ["<leader>ll"] = { "<cmd>Telescope lsp_definitions<cr>",
                         desc = "Search definitions" },
      ["<leader>tb"] = { function() require"astronvim.utils".toggle_term_cmd "btm" end,
                         desc = "ToggleTerm btm" },
      ["<leader>tt"] = { "<cmd>ToggleTerm size=10 direction=horizontal<cr>",
                         desc = "ToggleTerm horizontal split" },
      [";"] = { ":", desc = "Vim command" },
    },
    v = {
      ["<space>"] = { "<pagedown>", desc = "Scroll down" },
      ["<S-space>"] = { "<pageup>", desc = "Scroll up" },
      [";"] = { ":", desc = "Vim command" },
    },
    t = {
      ["<C-w><C-h>"] = { "<C-\\><C-n><C-w>h", desc = "Leave terminal" },
    },
  },

  options = {
    g = {
      loaded_node_provider = 0,
      loaded_perl_provider = 0,
      loaded_ruby_provider = 0,
      mapleader = ",",
      translator_target_lang = "ja",
    },
    opt = {
      fileencodings = { "utf-8", "cp932", "euc-jp", "sjis" },
      list = true,
      listchars = { tab = "▸-" },
      ttimeoutlen = 100,
    },
  },

  plugins = {
    {
      "jay-babu/mason-nvim-dap.nvim",
      opts = {
        ensure_installed = {'stylua', 'jq'},
        handlers = {
          function(config)
            -- all sources with no handler get passed here

            -- Keep original functionality
            require('mason-nvim-dap').default_setup(config)
          end,
          python = function(config)
            config.adapters = {
              type = "executable",
              command = string.format('%s/bin/python', os.getenv('VIRTUAL_ENV')),
              args = {
                "-m",
                "debugpy.adapter",
              },
            }
            require('mason-nvim-dap').default_setup(config) -- don't forget this!
          end,
        },
      },
    },
    { "catppuccin/nvim", name = "catppuccin", lazy = false, priority = 1000 },
    { "folke/tokyonight.nvim", lazy = false, priority = 1000 },
    { "sainnhe/gruvbox-material", lazy = false, priority = 1000 },
    { "ishan9299/modus-theme-vim", lazy = false, priority = 1000 },
    {
      "TimUntersberger/neogit",
      event = { "BufRead", "InsertEnter" },
      cmd = "Neogit",
      keys = {{
        "<leader>gt",
        "<cmd>Neogit<cr>",
        desc = "Neogit status",
      }},
      dependencies = "nvim-lua/plenary.nvim",
      config = true,
    },
    {
      "nvim-treesitter/nvim-treesitter",
      dependencies = {
        "RRethy/nvim-treesitter-endwise",
        "mrjones2014/nvim-ts-rainbow",
        "andymass/vim-matchup",
      },
      event = "VeryLazy",
      opts = {
        endwise = {
          enable = true,
        },
        ensure_installed = {
          "c",
          "lua",
          "markdown",
          "org",
          "query",
          "python",
          "tsx",
          "typescript",
          "vim",
        },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = { "org" },
        },
        indent = {
          enable = true,
        },
        matchup = {
          enable = true,
        },
        rainbow = {
          enable = true,
          extended_mode = true,
          max_file_lines = nil,
        },
      },
    },
    { 
      "tyru/open-browser.vim",
      keys = {{
        "<leader>x",
        "<Plug>(openbrowser-smart-search)",
        desc = "Open browser",
      }},
    },
    {
      "nvim-orgmode/orgmode", ft = "org",
      dependencies = { "nvim-treesitter/nvim-treesitter" },
      keys = {{
        "<leader>o", name = "Orgmode",
      },
      {
        "<leader>o.",
        '<cmd>lua require("orgmode").action("org_mappings.org_time_stamp")<cr>',
        desc = "org calendar",
      }},
      config = function()
        require('orgmode').setup_ts_grammar()
        require('orgmode').setup({
          org_blank_before_new_entry =
            { heading = false, plain_list_item = false },
          calendar_week_start_day = 0,
        })
      end,
    },
    {
      "nvim-treesitter/playground",
      dependencies = { "nvim-treesitter/nvim-treesitter" },
      event = "VeryLazy",
    },
    {
      "folke/trouble.nvim",
      event = { "BufRead", "InsertEnter" },
      dependencies = { "nvim-tree/nvim-web-devicons" },
      config = true,
    },
    { "machakann/vim-sandwich", event = "InsertEnter" },
    { "dstein64/vim-startuptime", cmd = "StartupTime" },
    {
      "voldikss/vim-translator",
      keys = {
        { "<leader>T", "<Plug>Translate", desc = "Translate" },
        { "<leader>W", "<Plug>TranslateW", desc = "TranslateW" },
        { "<leader>T", "<Plug>TranslateV", desc = "Translate", mode = "v" },
        { "<leader>W", "<Plug>TranslateWV", desc = "TranslateW", mode = "v" },
      },
    },
    { "vim-jp/vimdoc-ja", event = "InsertEnter", ft = "help" },
  },

  polish = function ()
    -- lsp highlight
    vim.cmd([[
    hi LspReferenceText gui=bold  ",underline
    hi LspReferenceRead gui=bold  ",underline
    hi LspReferenceWrite gui=bold ",underline
    ]])
    -- vim-ufo
    vim.cmd([[au FileType org UfoDetach]])
    -- ime
    vim.cmd([[
    augroup restore-ime
      autocmd!
      if exists("$TMUX")
        autocmd InsertEnter * silent call chansend(v:stderr, "\ePtmux;\e\e[<r\e\\")
        autocmd InsertLeave * silent call chansend(v:stderr, "\ePtmux;\e\e[<s\e\e[<0t\e\\")
        autocmd VimLeave * silent call chansend(v:stderr, "\ePtmux;\e\e[<0t\e\e[<s\e\\")
      else
        autocmd InsertEnter * silent call chansend(v:stderr, "\e[<r")
        autocmd InsertLeave * silent call chansend(v:stderr, "\e[<s\e[<0t")
        autocmd VimLeave * silent call chansend(v:stderr, "\e[<0t\e[<s")
      endif
    augroup END
    ]])
    -- python3_host_prog
    local prog = vim.fn.expand("~/.venv/bin/python3")
    if vim.fn.filereadable(prog) then
      vim.g.python3_host_prog = prog
    end
    -- keyword unit
    vim.cmd([[set iskeyword-=_]])
  end,
}
