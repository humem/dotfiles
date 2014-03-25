" vi互換ではなくVimのデフォルト設定にする
set nocompatible

" 日本語を扱うための設定
set encoding=utf8
set fileencoding=utf-8

" NeoBundleでプラグインを管理する
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
call neobundle#rc(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'

" 以下のプラグインをバンドル
" colorschemes
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'w0ng/vim-hybrid'
NeoBundle 'vim-scripts/twilight'
NeoBundle 'jonathanfilip/vim-lucius'
NeoBundle 'jpo/vim-railscasts-theme'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'vim-scripts/Wombat'
NeoBundle 'tomasr/molokai'
NeoBundle 'vim-scripts/rdark'
colorscheme jellybeans
NeoBundle 'Shougo/vimproc', {
  \ 'build' : {
    \ 'windows' : 'make -f make_mingw32.mak',
    \ 'cygwin' : 'make -f make_cygwin.mak',
    \ 'mac' : 'make -f make_mac.mak',
    \ 'unix' : 'make -f make_unix.mak',
  \ },
  \ }
NeoBundle 'shougo/vimshell'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'Shougo/neomru.vim'
" Unite
NeoBundle 'Shougo/unite.vim'
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable =1
let g:unite_source_file_mru_limit = 200
nnoremap <silent> ,uy :<C-u>Unite history/yank<CR>
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
nnoremap <silent> ,uu :<C-u>Unite file_mru buffer<CR>
"ステータスバーを表示
NeoBundle 'itchyny/lightline.vim'
"ステータスラインを常に表示する
set laststatus=2
"URLをブラウザで開く
NeoBundle 'open-browser.vim'
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)
" SVN
NeoBundle 'vcscommand.vim'
NeoBundle 'kmnk/vim-unite-svn'

filetype plugin indent on
NeoBundleCheck

" 表示系
" シンタックスハイライト
syntax on
" カーソル行をハイライト
set cursorline
" スクロールする時に下が見えるようにする
set scrolloff=5
" 行番号を表示
set number
" 右下に表示される行・列の番号を表示する
set ruler
" 検索結果をハイライト表示
set hlsearch
" Escの2回押しでハイライト消去
nmap <silent> <Esc><Esc> :nohlsearch<CR>

" ファイル・バッファ系
" .swapファイルを作らない
set noswapfile
" バックアップファイルを作らない
set nowritebackup
" バックアップをしない
set nobackup
" 変更中のファイルでも、保存しないで他のファイルを表示
"set hidden

" 操作系
" [Backspace] で既存の文字を削除できるように設定
" start - 既存の文字を削除できるように設定
" eol - 行頭で[Backspace]を使用した場合上の行と連結
" indent - オートインデントモードでインデントを削除できるように設定
set backspace=indent,eol,start
" 補完の際の大文字小文字の区別しない
set infercase
" 小文字の検索でも大文字も見つかるようにする
set ignorecase
" ただし大文字も含めた検索の場合はその通りに検索する
set smartcase
" インクリメンタルサーチを行う
set incsearch
" インデント
set cindent         "C言語のインデントに従って高度な自動インデントを行う
set expandtab       "Tabの代わりにSpaceを挿入する。Tabを打つ時は'Ctr-V Tab'。
set smarttab        "行頭の余白内でTabを打ち込むと、'shiftwidth'の数だけインデントする
set shiftwidth  =4
set tabstop     =4 "タブの文字数を設定する
set softtabstop =4 "ファイル内のTabが対応する空白の数
" Ctrl + hjkl でウィンドウ間を移動
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"スペースキーで一画面移動
nnoremap <SPACE>   <PageDown>
nnoremap <S-SPACE> <PageUp>
vnoremap <SPACE>   <C-d>
vnoremap <S-SPACE> <C-u>
"強制全保存終了を無効化。
"nnoremap ZZ <Nop>
" ; を : として使う
nnoremap ; :
vnoremap ; :

" ビープ音を消す
set vb t_vb=
