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
  { ";", ":" },
}
for i, km in ipairs(keymaps) do
  local mode = km.mode and km.mode or "n"
  local opts = km.opts and km.opts or {}
  vim.keymap.set(mode, km[1], km[2], opts)
end

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
  {
    "neoclide/coc.nvim", branch = "release",
    event = { "BufNewFile", "BufRead" }, 
    keys = {
      { "K", ":call ShowDocumentation()<cr>" },
      { "<leader>a", "<Plug>(coc-codeaction-selected)iw" },
      { "<leader>r", "<Plug>(coc-rename)" },
      { "<leader>.", "<Plug>(coc-definition)" },
      { "<leader>/", "<Plug>(coc-references)" },
    },
  },
  { "numToStr/Comment.nvim", keys = { "gc" }, config = true },
  {
    "lewis6991/gitsigns.nvim", config = true,
    event = { "BufNewFile", "BufRead" }, 
  },
  { "ellisonleao/gruvbox.nvim", lazy = false, priority = 1000 },
  { "RRethy/vim-illuminate", event = { "BufWinEnter" } },
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
      sections = {
        lualine_a = {
          "g:coc_git_blame",
          "g:coc_status",
          "bo:filetype",
        },
        lualine_b = {
          "g:coc_git_status",
          "diff",
          { "diagnostics", sources = { "coc" } },
        },
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
  { 'kevinhwang91/nvim-hlslens', event = "InsertEnter", config = true },
  {
    "petertriho/nvim-scrollbar", config = true,
    event = { "BufWinEnter", "TabEnter", "TermEnter", "WinEnter" },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = "VeryLazy",
    config = function()
      require('nvim-treesitter.configs').setup {
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = { "org" },
        },
        indent = {
          enabled = true,
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
  { "puremourning/vimspector", ft = "python" },
  { "simeji/winresizer", keys = { "<C-e>" } },
  { "folke/which-key.nvim", config = true, event = "InsertEnter" },
})

-- plugin settings

-- coc
vim.g.coc_global_extensions = {
  "coc-eslint8",
  "coc-git",
  "coc-highlight",
  "coc-lists",
  "coc-prettier",
  "coc-pyright",
  "coc-tsserver",
}

vim.cmd([[
function! ShowDocumentation() abort
  if index(['vim','lua','help'], &filetype) >= 0
    execute 'h ' . expand('<cword>')
  elseif coc#rpc#ready()
    call CocActionAsync('doHover')
  endif
endfunction

" highlight CocHighlightText gui=bold,underline
" autocmd CursorHold * silent call CocActionAsync('highlight')
]])

-- indent-blankline
vim.cmd([[highlight IndentBlanklineChar guifg=#708090 gui=nocombine]])

-- vim-translator
vim.g.translator_target_lang = "ja"

-- vimspector
vim.g.vimspector_enable_mappings = "HUMAN"
vim.g.vimspector_install_gadgets = { "debugpy" }

-- colorscheme
local ok, _ = pcall(require, "cs")
if not ok then
  vim.o.background = "dark"
  vim.cmd([[colorscheme gruvbox]])
end
