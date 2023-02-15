call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

Plug 'jiangmiao/auto-pairs'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tjdevries/colorbuddy.nvim'
Plug 'lambdalisue/fern.vim'
" Plug 'junegunn/fzf', {'dir': '~/.fzf_bin', 'do': './install --all'}
Plug 'lambdalisue/gina.vim'
Plug 'sainnhe/gruvbox-material'
Plug 'Yggdroot/indentLine'
Plug 'ishan9299/modus-theme-vim', {'branch': 'stable'}
" Plug 'preservim/nerdtree'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-orgmode/orgmode'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tomasiser/vim-code-dark'
" Plug 'ryanoasis/vim-devicons'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'vim-jp/vimdoc-ja'
Plug 'puremourning/vimspector'

call plug#end()

" set options
set termguicolors
set number

" map prefix
let g:mapleader = ","
nnoremap <Leader> <Nop>
xnoremap <Leader> <Nop>

noremap <Space> <PageDown>
noremap <S-Space> <PageUp>

nnoremap <Esc><Esc> :nohlsearch<CR>

inoremap <Esc> <Esc><Esc>

iab tilda ~
" iab backtick

let openuri_cmd = '!am start --user 0 -a android.intent.action.VIEW -t text/html -d %s'

"" airline
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline_theme = 'papercolor'
let g:airline#extensions#scrollbar#enabled = 1
let g:airline#extensions#scrollbar#minwidth = 100
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#whitespace#mixed_indent_algo = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

"" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.colnr = ':' ""' ℅:'
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ' ' ""' :'
let g:airline_symbols.maxlinenr = '' ""'☰ '
let g:airline_symbols.dirty = '*' ""'⚡'

"" coc.nvim
let g:coc_global_extensions = ['coc-pyright', 'coc-tsserver', 'coc-eslint8', 'coc-prettier', 'coc-git', 'coc-lists']

inoremap <silent> <expr>    <C-Space> coc#refresh()
nnoremap <silent> K         :<C-u>call <SID>show_documentation()<CR>
nmap     <silent> <Leader>r <Plug>(coc-rename)
nmap     <silent> <Leader>a <Plug>(coc-codeaction-selected)iw
nmap     <silent> <Leader>. <Plug>(coc-definition)
nmap     <silent> <Leader>/ <Plug>(coc-references)

function! s:coc_typescript_settings() abort
  nnoremap <silent> <buffer> [dev]f :<C-u>CocCommand eslint.executeAutofix<CR>:CocCommand prettier.formatFile<CR>
endfunction

augroup coc_ts
  autocmd!
  autocmd FileType typescript,typescriptreact call <SID>coc_typescript_settings()
augroup END

function! s:show_documentation() abort
  if index(['vim','help'], &filetype) >= 0
    execute 'h ' . expand('<cword>')
  elseif coc#rpc#ready()
    call CocActionAsync('doHover')
  endif
endfunction

""" completion
set completeopt=menuone,preview,noinsert
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>"  : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>"  : "\<S-Tab>"
inoremap <expr> <CR>    pumvisible() ? "\<C-y>"  : "\<CR>"
inoremap <expr> <C-n>   pumvisible() ? "\<Down>" : "\<C-n>"
inoremap <expr> <C-p>   pumvisible() ? "\<Up>"   : "\<C-p>"

"" fern
nnoremap <silent> <Leader>e :<C-u>Fern . -drawer<CR>
nnoremap <silent> <Leader>E :<C-u>Fern . -drawer -reveal=%<CR>
let g:fern#default_hidden = 1

"" indentLine
let g:indentLine_color_term =239
let g:indentLine_color_gui = '#708090'
let g:indentLine_char = '¦'

"" orgmode + treesitter
lua << EOF
-- Load custom treesitter grammar for org filetype
require('orgmode').setup_ts_grammar()

-- Treesitter configuration
require('nvim-treesitter.configs').setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop,
  -- highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    -- Required for spellcheck, some LaTex highlights and
    -- code block highlights that do not have ts grammar
    additional_vim_regex_highlighting = {'org'},
  },
  ensure_installed = {
	  "org", -- Or run :TSUpdate org
	  "typescript",
	  "tsx"},
}

require('orgmode').setup({
  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
  org_default_notes_file = '~/Dropbox/org/refile.org',
})
EOF

nnoremap <Leader>c <Cmd>lua require("orgmode").action("org_mappings.org_time_stamp")<CR>

"" vimspector
let g:vimspector_enable_mappings = 'HUMAN'
let g:vimspector_install_gadgets = ['debugpy']

" colorscheme gruvbox-material
colorscheme modus-operandi
