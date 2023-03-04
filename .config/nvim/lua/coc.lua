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
  { "puremourning/vimspector", ft = "python" },
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

highlight CocHighlightText gui=bold,underline
autocmd CursorHold * silent call CocActionAsync('highlight')
]])

-- vimspector
vim.g.vimspector_enable_mappings = "HUMAN"
vim.g.vimspector_install_gadgets = { "debugpy" }
