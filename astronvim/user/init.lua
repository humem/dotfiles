return {
  -- set iskeyword-=_
  -- iab tilda ~
  -- iab backtick `
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
      [";"] = { ":", desc = "Vim command" },
    },
    v = {
      ["<space>"] = { "<pagedown>", desc = "Scroll down" },
      ["<S-space>"] = { "<pageup>", desc = "Scroll up" },
      [";"] = { ":", desc = "Vim command" },
    },
    t = {
      ["<C-c>"] = { "<C-\\><C-n>", desc = "Detach terminal" },
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
      autochdir = true,
      fileencodings = { "utf-8", "cp932", "euc-jp", "sjis" },
    },
  },

  plugins = {
    {
      "jay-babu/mason-nvim-dap.nvim",
      opts = {
        ensure_installed = { "python" }
      },
    },
    { "ishan9299/modus-theme-vim", lazy = false, priority = 1000 },
    {
      "nvim-treesitter/nvim-treesitter",
      event = "VeryLazy",
      opts = {
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = { "org" },
        },
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
    { "kevinhwang91/nvim-ufo", enabled = false },
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
        "<leader>oC",
        '<cmd>lua require("orgmode").action("org_mappings.org_time_stamp")<cr>',
        desc = "org Calendar",
      }},
      config = function()
        require('orgmode').setup_ts_grammar()
        require('orgmode').setup({
          calendar_week_start_day = 0,
        })
      end,
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
    local prog = vim.fn.expand("~/.venv/bin/python3")
    if vim.fn.filereadable(prog) then
      vim.g.python3_host_prog = prog
    end

    --- https://qiita.com/sff1019/items/3f73856b78d7fa2731c7
    vim.cmd([[
      function! s:get_syn_id(transparent)
        let synid = synID(line("."), col("."), 1)
        if a:transparent
          return synIDtrans(synid)
        else
          return synid
        endif
      endfunction
      function! s:get_syn_attr(synid)
        let name = synIDattr(a:synid, "name")
        let ctermfg = synIDattr(a:synid, "fg", "cterm")
        let ctermbg = synIDattr(a:synid, "bg", "cterm")
        let guifg = synIDattr(a:synid, "fg", "gui")
        let guibg = synIDattr(a:synid, "bg", "gui")
        return {
              \ "name": name,
              \ "ctermfg": ctermfg,
              \ "ctermbg": ctermbg,
              \ "guifg": guifg,
              \ "guibg": guibg}
      endfunction
      function! s:get_syn_info()
        let baseSyn = s:get_syn_attr(s:get_syn_id(0))
        echo "name: " . baseSyn.name .
              \ " ctermfg: " . baseSyn.ctermfg .
              \ " ctermbg: " . baseSyn.ctermbg .
              \ " guifg: " . baseSyn.guifg .
              \ " guibg: " . baseSyn.guibg
        let linkedSyn = s:get_syn_attr(s:get_syn_id(1))
        echo "link to"
        echo "name: " . linkedSyn.name .
              \ " ctermfg: " . linkedSyn.ctermfg .
              \ " ctermbg: " . linkedSyn.ctermbg .
              \ " guifg: " . linkedSyn.guifg .
              \ " guibg: " . linkedSyn.guibg
      endfunction
      command! SyntaxInfo call s:get_syn_info()
    ]])
  end,
}
