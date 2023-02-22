call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

Plug 'jiangmiao/auto-pairs'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tjdevries/colorbuddy.nvim'
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/gina.vim'
Plug 'lambdalisue/nerdfont.vim'
Plug 'lambdalisue/glyph-palette.vim'
" Plug 'junegunn/fzf', {'dir': '~/.fzf_bin', 'do': './install --all'}
Plug 'sainnhe/gruvbox-material'
Plug 'Yggdroot/indentLine'
Plug 'ishan9299/modus-theme-vim', {'branch': 'stable'}
Plug 'TimUntersberger/neogit'
" Plug 'preservim/nerdtree'
" Plug 'prichrd/netrw.nvim'
" Plug 'nvim-tree/nvim-web-devicons'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-orgmode/orgmode'
" Plug 'Xuyuanp/scrollbar.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.1' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tomasiser/vim-code-dark'
" Plug 'ryanoasis/vim-devicons'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'machakann/vim-sandwich'
Plug 'vim-jp/vimdoc-ja'
Plug 'puremourning/vimspector'

call plug#end()

" set options
set clipboard+=unnamedplus
set cursorline
set expandtab
set ignorecase
set list listchars=tab:\▸\-
set number
set shiftwidth=2
set smartcase
set tabstop=2
set termguicolors

" map prefix
let g:mapleader = ","
nnoremap <Leader> <Nop>
xnoremap <Leader> <Nop>

nnoremap <Leader>R :source $MYVIMRC<CR>

noremap <Space>   <PageDown>
noremap <S-Space> <PageUp>

nnoremap <Esc><Esc> :nohlsearch<CR><Esc>

map  <C-g> <Esc>
map! <C-g> <Esc>

" 折り返し時に表示行単位での移動できるようにする
nnoremap j gj
nnoremap k gk

" inoremap <Esc> <Esc><Esc>

iab tilda ~
iab backtick `

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
" inoremap <expr> <CR>    pumvisible() ? "\<C-y>"  : "\<CR>"
" inoremap <expr> <C-n>   pumvisible() ? "\<Down>" : "\<C-n>"
" inoremap <expr> <C-p>   pumvisible() ? "\<Up>"   : "\<C-p>"

"" fern
nnoremap <silent> <Leader>e :<C-u>Fern . -drawer<CR>
nnoremap <silent> <Leader>E :<C-u>Fern . -drawer -reveal=%<CR>
let g:fern#default_hidden = 1
let g:fern#renderer = 'nerdfont'
" アイコンに色をつける
augroup my-glyph-palette
  autocmd! *
  autocmd FileType fern call glyph_palette#apply()
  autocmd FileType nerdtree,startify call glyph_palette#apply()
augroup END

" function! s:open(filename, ...) abort
"   let options = copy(a:0 ? a:1 : {})
"   return s:Process.start([
"         \ 'open',
"         \ a:filename,
"         \], options)
" endfunction

"" indentLine
let g:indentLine_color_term =239
let g:indentLine_color_gui = '#708090'
let g:indentLine_char = '¦'

"" neogit"
lua require('neogit').setup {}

"" netrw"
" https://vonheikemen.github.io/devlog/tools/using-netrw-vim-builtin-file-explorer/
" Open Netrw on the directory of the current file
nnoremap <leader>dd :Lexplore %:p:h<CR>

" Toggle the Netrw window
nnoremap <Leader>da :Lexplore<CR>

if &columns < 90
  " If the screen is small, occupy half
  let g:netrw_winsize = 50
else
  " else take 30%
  let g:netrw_winsize = 30
endif

" Sync current directory and browsing directory
" This solves the problem with the 'move' command
let g:netrw_keepdir = 0

" Hide banner
let g:netrw_banner = 0

" Hide dotfiles
" let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'

" A better copy command
let g:netrw_localcopydircmd = 'cp -r'

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
  " Close Netrw window
  nmap <buffer> <leader>dd :Lexplore<CR>

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

" https://qiita.com/gorilla0513/items/bf2f78dfec67242f5bcf
" ファイルツリーの表示形式、1にするとls -laのような表示になります
let g:netrw_liststyle=1
" ヘッダを非表示にする
" let g:netrw_banner=0
" サイズを(K,M,G)で表示する
let g:netrw_sizestyle="H"
" 日付フォーマットを yyyy-mm-dd hh:mm:ss で表示する
let g:netrw_timefmt="%Y-%m-%d %H:%M:%S"
" プレビューウィンドウを垂直分割で表示する
" let g:netrw_preview=1

" lua << EOF
" require'netrw'.setup{
"   -- Put your configuration here, or leave the object empty to take the default
"   -- configuration.
"   icons = {
"     symlink = '', -- Symlink icon (directory and file)
"     directory = '', -- Directory icon
"     file = '', -- File icon
"   },
"   use_devicons = true, -- Uses nvim-web-devicons if true, otherwise use the file icon specified above
"   mappings = {}, -- Custom key mappings
" }
" EOF
" 
" "" nvim-web-devicons
" lua require'nvim-web-devicons'.setup()

"" orgmode + treesitter
lua << EOF
require('orgmode').setup_ts_grammar()

require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = {'org'},
  },
  ensure_installed = {
    "c",
    "help",
    "lua",
    "vim",
    "org",
    "typescript",
    "tsx"},
}

require('orgmode').setup({
  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
  org_default_notes_file = '~/Dropbox/org/refile.org',
})
EOF

nnoremap <Leader>c <Cmd>lua require("orgmode").action("org_mappings.org_time_stamp")<CR>

" "" scrollbar
" augroup ScrollbarInit
"   autocmd!
"   autocmd WinScrolled,VimResized,QuitPre * silent! lua require('scrollbar').show()
"   autocmd WinEnter,FocusGained           * silent! lua require('scrollbar').show()
"   autocmd WinLeave,BufLeave,BufWinLeave,FocusLost            * silent! lua require('scrollbar').clear()
" augroup end

"" telescope"
nnoremap <leader>ff <cmd>Telescope find_files hidden=true<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

"" vimspector
let g:vimspector_enable_mappings = 'HUMAN'
let g:vimspector_install_gadgets = ['debugpy']

" colorscheme gruvbox-material
colorscheme modus-operandi
