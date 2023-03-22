-- vim.opt
local vim_options = {
  autochdir = true,
  clipboard = "unnamedplus",
  completeopt = { "menuone", "noinsert", "preview" },
  cursorline = true,
  expandtab = true,
  fileencodings = { "utf-8", "cp932", "euc-jp", "sjis" },
  ignorecase = true,
  laststatus = 2,
  list = true,
  listchars = { tab = "▸-" },
  number = true,
  shiftwidth = 2,
  smartcase = true,
  swapfile = false,
  tabstop = 2,
  termguicolors = true,
  updatetime = 300,
}
for k, v in pairs(vim_options) do
  vim.opt[k] = v
end

-- keymaps
vim.g.mapleader = ","

local keymaps = {
  { "<c-g>", "<esc>", mode = { "n", "i", "v", "x" }, opts = { remap = true } },
  { "<esc><esc>", ":nohlsearch<cr><esc>" },
  { "<leader>d", ":Explore<cr>" },
  { "<leader>l", ":call EditResolved('%:p')<cr>" },
  { "<leader>R", ":source $MYVIMRC<cr>" },
  { "<leader>q", ":qa<cr>" },
  { "<leader>w", ":w<cr>" },
  { "<space>", "<pagedown>", mode = { "n", "v" } },
  { "<s-space>", "<pageup>", mode = { "n", "v" } },
  { "jj", "<esc>", mode = "i" },
  { "j", "gj" },
  { "k", "gk" },
  { ";", ":", mode = { "n", "v" } },
}

local function set_keymap(keymaps)
  for i, km in ipairs(keymaps) do
    local mode = km.mode and km.mode or "n"
    local opts = km.opts and km.opts or {}
    vim.keymap.set(mode, km[1], km[2], opts)
  end
end

set_keymap(keymaps)

vim.cmd([[
function! EditResolved(filename) abort
  let l:resolved = fnamemodify(resolve(expand(a:filename)), ":p:h")
  echo l:resolved
  execute 'edit ' . fnameescape(l:resolved)
endfunction
]])

-- provider
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
local prog = vim.fn.expand("~/.venv/bin/python3")
if vim.fn.filereadable(prog) then
  vim.g.python3_host_prog = prog
end

-- abbreviations
vim.cmd([[
iab tilda ~
iab backtick `
]])

-- netrw
vim.g.netrw_banner = 0
vim.g.netrw_keepdir = 0
vim.g.netrw_keepj = ""
vim.g.netrw_liststyle = 1
vim.g.netrw_localcopydircmd = "cp -r"
vim.g.netrw_sizestyle = "H"
vim.g.netrw_timefmt = "%Y-%m-%d %H:%M:%S"

vim.cmd([[
" Delete a non-empty directory
function! NetrwRemoveRecursive()
  if &filetype ==# 'netrw'
    " Prepare the delete command.
    " Make it so that is triggered by just pressing Enter
    cnoremap <buffer> <CR> rm -r<CR>
    " Unmark all files (don't want to delete anything by accident)
    normal mu
    " Mark the file/directory under the cursor
    normal mf
    " Show the prompt to enter the command
    " In here you either press Enter to confirm
    " or press ctrl + c to abort.
    " Don't do anything else!
    try
      normal mx
    catch
      echo "Canceled"
    endtry
    " Undo the Enter keymap
    cunmap <buffer> <CR>
  endif
endfunction

" Toggle sort-by
function! NetrwToggleSortBy()
  if g:netrw_sort_by == "name"
    let g:netrw_sort_by = "time"
    let g:netrw_sort_direction = "reverse"
    " echo "time reverse"
  else
    let g:netrw_sort_by = "name"
    let g:netrw_sort_direction = "normal"
    " echo "name"
  endif
endfunction

function! NetrwShowSortBy()
  echo g:netrw_sort_by g:netrw_sort_direction
endfunction

" Better keymaps for Netrw
function! NetrwMapping()
  " Go to file and close Netrw window
  nmap <buffer> L <CR>:Lexplore<CR>
  " Go back in history
  nmap <buffer> H u
  " Go up a directory
  nmap <buffer> h -^
  " Go down a directory / open file
  nmap <buffer> l <CR>
  " Toggle dotfiles
  nmap <buffer> . gh
  " Toggle the mark on a file
  nmap <buffer> <TAB> mf
  " Unmark all files in the buffer
  nmap <buffer> <S-TAB> mF
  " Unmark all files
  nmap <buffer> <Leader><TAB> mu
  " 'Bookmark' a directory
  nmap <buffer> bb mb
  " Delete the most recent directory bookmark
  nmap <buffer> bd mB
  " Got to a directory on the most recent bookmark
  nmap <buffer> bl gb
  " Create a file
  nmap <buffer> ff %:w<CR>:buffer #<CR>
  " Rename a file
  nmap <buffer> fe R
  " Copy marked files
  nmap <buffer> fc mc
  " Copy marked files in the directory under cursor
  nmap <buffer> fC mtmc
  " Move marked files
  nmap <buffer> fx mm
  " Move marked files in the directory under cursor
  nmap <buffer> fX mtmm
  " Execute a command on marked files
  nmap <buffer> f; mx
  " Show the list of marked files
  nmap <buffer> fl :echo join(netrw#Expose("netrwmarkfilelist"), "\n")<CR>
  " Show the current target directory
  nmap <buffer> fq :echo 'Target:' . netrw#Expose("netrwmftgt")<CR>
  " Set the directory under the cursor as the current target
  nmap <buffer> fd mtfq
  " Delete a file
  nmap <buffer> FF :call NetrwRemoveRecursive()<CR>
  " Close the preview window
  nmap <buffer> P <C-w>z
  " Toggle sort-by among name and reversed time
  nmap <silent><buffer> s :call NetrwToggleSortBy()<CR><C-L>:call NetrwShowSortBy()<CR>
endfunction

augroup netrw_mapping
    autocmd!
    autocmd filetype netrw call NetrwMapping()
augroup END
]])

-- lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  { 'neovim/nvim-lspconfig', event = { "BufRead", "InsertEnter" } },
  { 'williamboman/mason.nvim', config = true, event = { "BufRead", "InsertEnter" } },
  {
    'williamboman/mason-lspconfig',
    dependencies = {
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-nvim-lsp",
    },
    event = { "BufRead", "InsertEnter" },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          'eslint',
          'pyright',
          'tsserver',
        },
        automatic_installation = true,
      })
      local lspconfig = require("lspconfig")
      require("mason-lspconfig").setup_handlers({
        function(server_name)
          local opts = {
            capabilities = require("cmp_nvim_lsp").default_capabilities(),
          }
          lspconfig[server_name].setup(opts)
        end,
      })
    end,
  },
  {
    'jose-elias-alvarez/null-ls.nvim', event = { "BufRead", "InsertEnter" },
    config = function()
      local null_ls = require("null-ls")
      null_ls.setup({
        sources = { null_ls.builtins.formatting.prettier },
      })
    end,
  },
  {
    'jayp0521/mason-null-ls.nvim', event = { "BufRead", "InsertEnter" },
    opts = {
      ensure_installed = { 'prettier' },
      automatic_installation = true,
    },
  },
  { 'stevearc/dressing.nvim', config = true, event = { "BufRead", "InsertEnter" } },
  { 
    'tami5/lspsaga.nvim',
    event = { "BufRead", "InsertEnter" },
    config = function()
      require("lspsaga").setup()
      vim.api.nvim_create_autocmd({ "CursorHold" }, {
        pattern = { "*" },
        callback = function()
          require("lspsaga.diagnostic").show_cursor_diagnostics()
        end,
      })
    end,
  },
  {
    'ray-x/lsp_signature.nvim',
    opts = { hint_enable = false },
    event = { "BufRead", "InsertEnter" },
  },
  { 'onsails/lspkind-nvim', event = { "BufRead", "InsertEnter" } },
  { 'j-hui/fidget.nvim', config = true },
  {
    'hrsh7th/nvim-cmp',
    event = { "BufRead", "InsertEnter" }, 
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
    },
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        enabled = true,
        mapping = cmp.mapping.preset.insert({
          ["<C-u>"] = cmp.mapping.scroll_docs(-4),
          ["<C-d>"] = cmp.mapping.scroll_docs(4),
          -- ["<tab>"] = cmp.mapping.complete(),
          ["<C-y>"] = cmp.mapping.confirm({ select = true }),
        }),
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "nvim_lsp_signature_help" },
          { name = "buffer" },
          { name = "path" },
          { name = "cmdline" },
          { name = "git" },
        }),
        formatting = {
          fields = { "abbr", "kind", "menu" },
          format = require("lspkind").cmp_format({
            mode = "text",
          }),
        },
      })
    end,
  },
  { "mfussenegger/nvim-dap", ft = { "python" } },
  {
    "rcarriga/nvim-dap-ui", ft = { "python" },
    opts = {
      icons = { expanded = "", collapsed = "" },
      layouts = {
        {
          elements = {
            { id = "watches", size = 0.20 },
            { id = "stacks", size = 0.20 },
            { id = "breakpoints", size = 0.20 },
            { id = "scopes", size = 0.40 },
          },
          size = 64,
          position = "right",
        },
        {
          elements = {
            "repl",
            "console",
          },
          size = 0.20,
          position = "bottom",
        },
      },
    },
  },
  {
    "mfussenegger/nvim-dap-python",
    ft = { "python" },
    config = function()
      local venv = os.getenv('VIRTUAL_ENV')
      command = string.format('%s/bin/python', venv)
      require('dap-python').setup(command)
    end,
  },

  {
    "Vonr/align.nvim",
    event = "InsertEnter",
    config = function()
      local keymaps = {
        { "<leader>aa", ':lua require("align").align_to_char(1, true)<cr>' },
        { "<leader>as", ':lua require("align").align_to_char(2, true, true)<cr>' },
        { "<leader>aw", ':lua require("align").align_to_string(false, true, true)<cr>' },
        { "<leader>ar", ':lua require("align").align_to_string(true, true, true)<cr>' },
      }
      for i, km in ipairs(keymaps) do
        vim.keymap.set("x", km[1], km[2])
      end
    end,
  },
  {
    "numToStr/Comment.nvim",
    event = "InsertEnter", keys = { "gc" }, config = true,
  },
  {
    "lewis6991/gitsigns.nvim", config = true,
    event = { "BufNewFile", "BufRead" }, 
  },
  { "ellisonleao/gruvbox.nvim", lazy = false, priority = 1000 },
  {
    "RRethy/vim-illuminate",
    event = { "BufRead" },
    config = function()
      require('illuminate').configure({
        filetypes_allowlist = { "lua", "python", "vim" },
      })
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufRead",
    opts = {
      char = "¦",
      char_highlight_list = { "IndentBlanklineChar" },
    },
  },
  {
    "nvim-lualine/lualine.nvim", event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        theme = "PaperColor",
      },
    },
  },
  { "ishan9299/modus-theme-vim", lazy = false, priority = 1000 },
  {
    "TimUntersberger/neogit", config = true,
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {{ "<leader>g", "<cmd>Neogit cwd=%:p:h<cr>" }},
  },
  { "windwp/nvim-autopairs", event = "InsertEnter", config = true },
  {
    "norcalli/nvim-colorizer.lua",
    event = "BufRead",
    config = function()
      require("colorizer").setup()
    end,
  },
  { 'kevinhwang91/nvim-hlslens', event = "InsertEnter", config = true },
  {
    "petertriho/nvim-scrollbar", config = true,
    event = { "BufWinEnter", "TabEnter", "TermEnter", "WinEnter" },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "RRethy/nvim-treesitter-endwise",
      "p00f/nvim-ts-rainbow",
      "andymass/vim-matchup",
      "nvim-treesitter/nvim-treesitter-context",
    },
    build = ":TSUpdate",
    event = "VeryLazy",
    config = function()
      require('nvim-treesitter.configs').setup {
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = { "org" },
        },
        indent = {
          enable = true,
        },
        endwise = {
          enable = true,
        },
        rainbow = {
          enable = true,
          extended_mode = true,
          max_file_lines = nil,
        },
        matchup = {
          enable = true,
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
      }
    end,
  },
  { 
    "tyru/open-browser.vim",
    keys = {{ "<leader>x",  "<Plug>(openbrowser-smart-search)" }},
  },
  {
    "nvim-orgmode/orgmode", ft = "org",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    keys = {
      { "<leader>c", '<cmd>lua require("orgmode").action("org_mappings.org_time_stamp")<cr>' }
    },
    config = function()
      require('orgmode').setup_ts_grammar()
      require('orgmode').setup({
        calendar_week_start_day = 0,
      })
    end,
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-file-browser.nvim",
      "nvim-telescope/telescope-frecency.nvim",
      "kkharji/sqlite.lua",
      "nvim-tree/nvim-web-devicons",
    },
    cmd = { "Telescope" },
    keys = {
      { "<leader>b", "<cmd>Telescope frecency<cr>" },
      { "<leader>B", "<cmd>Telescope buffers<cr>" },
      { "<leader>D", "<cmd>Telescope file_browser<cr>" },
      { "<leader>f", "<cmd>Telescope find_files hidden=true<cr>" },
      { "<leader>G", "<cmd>Telescope live_grep<cr>" },
      { "<leader>h", "<cmd>Telescope help_tags<cr>" },
      { "<leader>j", "<cmd>Telescope jumplist<cr>" },
    },
    config = function()
      require("telescope").load_extension "file_browser"
      require("telescope").load_extension "frecency"
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
      { "<leader>T", "<Plug>Translate" },
      { "<leader>t", "<Plug>TranslateW" },
      { "<leader>T", "<Plug>TranslateV", mode = "v" },
      { "<leader>t", "<Plug>TranslateWV", mode = "v" },
    },
  },
  { "vim-jp/vimdoc-ja", event = "InsertEnter", ft = "help" },
  { "simeji/winresizer", keys = { "<C-e>" } },
  { "folke/which-key.nvim", config = true, event = "InsertEnter" },
})

-- plugin settings

-- lsp
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "typescript", "typescriptreact", "typescript.tsx" },
  callback = function()
    vim.keymap.set({ "n" }, "ff", function()
      vim.cmd([[EslintFixAll]])
      vim.lsp.buf.format({ name = "null-ls" })
    end)
  end,
})

local function show_documentation()
  local ft = vim.opt.filetype._value
  if ft == "vim" or ft == "lua" or ft == "help" then
    vim.cmd([[execute "h " . expand("<cword>") ]])
  else
    require("lspsaga.hover").render_hover_doc()
  end
end

local keymaps = {
  { "K", show_documentation },
  { "<leader>a", ':lua require("lspsaga.codeaction").code_action<cr>' },
  { "<leader>i", "<Cmd>Telescope diagnostics<CR>" },
  { "<leader>r", ':lua require("lspsaga.rename").rename<cr>' },
  { "<leader>]", ':lua require("lspsaga.diagnostic").navigate("next")<cr>' },
  { "<leader>[", ':lua require("lspsaga.diagnostic").navigate("prev")<cr>' },
  { "<leader>=", ':lua vim.lsp.buf.format<cr>' },
  { "<leader>.", "<Cmd>Telescope lsp_definitions<CR>" },
  { "<leader>/", "<Cmd>Telescope lsp_references<CR>" },
}
set_keymap(keymaps)

-- dap
local keymaps = {
  { "<F5>", ":DapContinue<CR>" },
  { "<F10>", ":DapStepOver<CR>" },
  { "<F11>", ":DapStepInto<CR>" },
  { "<F12>", ":DapStepOut<CR>" },
  { "<leader>zb", ":DapToggleBreakpoint<CR>" },
  { "<leader>zB", ':lua require("dap").set_breakpoint(nil, nil, vim.fn.input("Breakpoint condition: "))<CR>' },
  { "<leader>zl", ':lua require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))<CR>' },
  { "<leader>zo", ':lua require("dap").repl.open()<CR>' },
  { "<leader>zr", ':lua require("dap").run_last()<CR>' },
  { "<leader>z", ':lua require("dapui").toggle()<CR>' },
  { "<leader>zk", ':lua require("dapui").eval()<CR>' },
}
set_keymap(keymaps)

-- indent-blankline
vim.cmd([[highlight IndentBlanklineChar guifg=#708090 gui=nocombine]])

-- vim-illuminate
vim.cmd([[
augroup illuminate_augroup
    autocmd!
    autocmd VimEnter * hi IlluminatedWordRead gui=bold,underline
    autocmd VimEnter * hi IlluminatedWordText gui=bold,underline
    autocmd VimEnter * hi IlluminatedWordWrite gui=bold,underline
augroup END
]])

-- vim-translator
vim.g.translator_target_lang = "ja"

-- colorscheme
local ok, _ = pcall(require, "cs")
if not ok then
  vim.o.background = "dark"
  vim.cmd([[colorscheme gruvbox]])
end
