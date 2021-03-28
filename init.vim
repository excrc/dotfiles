set nocompatible

call plug#begin('~/.vim/plugged')


" tree-sitter
Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'nvim-treesitter/playground'

Plug 'nathanaelkane/vim-indent-guides'

Plug 'ChristianChiarulli/nvcode-color-schemes.vim'
Plug 'aonemd/kuroi.vim'
Plug 'dart-lang/dart-vim-plugin'
Plug 'StanAngeloff/php.vim'
Plug 'ap/vim-css-color'
Plug 'mattn/emmet-vim'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'prettier/vim-prettier'
Plug 'rhysd/vim-clang-format', { 'for': ['c', 'cpp'] }
Plug 'altercation/vim-colors-solarized'
Plug 'pangloss/vim-javascript'
Plug 'neovimhaskell/haskell-vim'
Plug 'itchyny/lightline.vim'

call plug#end()

filetype plugin indent on

syntax on

set linespace=-1
set autoindent
set hidden
set cursorline
set autoread
set autowrite
set autowriteall
set wildmenu
set showcmd
set number 
set relativenumber
set ruler
set nostartofline
set laststatus=2

set shiftwidth=2
set tabstop=2
set softtabstop=2
set expandtab
set smarttab

set incsearch
set hlsearch
set ignorecase
set smartcase
set confirm
set visualbell
set t_vb=
set mouse=a
set noswapfile
set nobackup
set nowritebackup
set nopaste
set cmdheight=1
set updatetime=300
set shortmess+=c
set formatoptions-=cro
set clipboard=unnamedplus
set wrap
set linebreak
set background=dark
set wildmenu
set conceallevel=1
set noshowmode
set bg=dark
set tgc

map <C-d> :NERDTreeToggle<CR>

" enable tree-sitter
lua require'nvim-treesitter.configs'.setup { highlight = { enable = true } }

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }

colorscheme nvcode
