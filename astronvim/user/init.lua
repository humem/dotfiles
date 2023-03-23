return {
  colorscheme = "modus-operandi",

  mappings = {
    n = {
      ["<space>"] = { "<pagedown>", desc = "Scroll down" },
      ["<S-space>"] = { "<pageup>", desc = "Scroll up" },
      ["<leader>Q"] = { ":qa<cr>", desc = "Quit all" },
    },
    v = {
      ["<space>"] = { "<pagedown>", desc = "Scroll down" },
      ["<S-space>"] = { "<pageup>", desc = "Scroll up" },
    },
    t = {
      ["<C-c>"] = { "<C-\\><C-n>", desc = "Detach terminal" },
    },
  },

  options = {
    g = {
      foldenable = false,
      loaded_node_provider = 0,
      loaded_perl_provider = 0,
      loaded_ruby_provider = 0,
      mapleader = ",",
      translator_target_lang = "ja",
    },
    opt = {
      fileencodings = { "utf-8", "cp932", "euc-jp", "sjis" },
    }
  },

  plugins = {
    { "ishan9299/modus-theme-vim", lazy = false, priority = 1000 },
    {
      "nvim-treesitter/nvim-treesitter",
      opts = {
        ensure_installed = {
          "c",
          "help",
          "lua",
          "org",
          "python",
          "tsx",
          "typescript",
          "vim",
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
        "<leader>Oc",
        '<cmd>lua require("orgmode").action("org_mappings.org_time_stamp")<cr>',
        desc = "Calendar",
      }},
      config = function()
        require('orgmode').setup_ts_grammar()
        require('orgmode').setup({
          calendar_week_start_day = 0,
        })
      end,
    },
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
    local prog = vim.fn.expand("~/.venv/bin/python3")
    if vim.fn.filereadable(prog) then
      vim.g.python3_host_prog = prog
    end
  end,
}
