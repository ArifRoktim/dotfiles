" These settings only needed for vim
if has('nvim')
    finish
endif

set nocompatible
filetype plugin indent on
if !exists("g:syntax_on")
    syntax enable
endif

set autoindent
set autoread
set background=dark
set backspace="indent,eol,start"
set belloff="all"
set complete-=i
set cscopeverbose
set display="lastline,msgsep"
set encoding=utf-8
set fillchars="vert:|,fold:·"
set formatoptions="tcqj"
set nofsync
set history=10000
set hlsearch
set incsearch
set langnoremap
set laststatus=2
set listchars="tab:> ,trail:-,nbsp:+"
set nrformats="bin,hex"
set ruler
set sessionoptions-=options
set shortmess+=F
set shortmess-=S
set showcmd
set sidescroll=1
set smarttab
set tabpagemax=50
set ttimeoutlen=50
set ttyfast
set viminfo+=!
set wildmenu
set wildoptions="pum,tagfile"
