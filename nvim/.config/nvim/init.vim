"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"    => dein Scripts
"    => Plugin Settings
"    => General
"    => User Interface
"    => Status line
"    => Tabs and spaces
"    => Tabs,windows,buffers
"    => Mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ========== dein Scripts ==========
set nocompatible

set runtimepath+=/home/arif/.config/nvim/dein//repos/github.com/Shougo/dein.vim

if dein#load_state('/home/arif/.config/nvim/dein/')
  call dein#begin('/home/arif/.config/nvim/dein/')

  call dein#add('/home/arif/.config/nvim/dein//repos/github.com/Shougo/dein.vim')

  " Add or remove plugins here:
  call dein#add('neomake/neomake')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('artur-shaik/vim-javacomplete2')

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

if dein#check_install()
  call dein#install()
  call dein#update()
endif

" ========== Plugin Settings ==========

" ===== Deoplete =====
" Enable autocomplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 2

" ===== vim-javacomplete2 =====
" Set omnifunc
autocmd FileType java setlocal omnifunc=javacomplete#Complete

" ===== Neomake =====
" Lint files on write
autocmd! BufWritePost * Neomake

" ========== General ==========

" Easily edit this file
command E edit ~/.config/nvim/init.vim

set history=500                                 " Sets how many lines of history VIM has to remember
set autoread                                    " Set to auto read when a file is changed from the outside
au FocusGained,BufEnter * :silent! checktime    " Autoread is bugged. Force it to update buffer
let mapleader = ","

" :W sudo saves the file
command W w !sudo tee % > /dev/null

set encoding=utf8               " Set utf8 as standard encoding
set ffs=unix,dos,mac            " Use correct EOL format
set nobackup                    " No backup files
set clipboard^=unnamedplus      " Set default register to system clipboard

" ========== User Interface ==========

colorscheme desert
set t_Co=256                " Enable 256 colors
highlight clear SignColumn

" Toggle highlighting current line
hi CursorLine cterm=NONE ctermbg=darkblue
nnoremap <Leader>c :set cursorline!<cr>

" Change color of popup menu
hi Pmenu    ctermbg=darkcyan ctermfg=black
hi PmenuSel ctermbg=blue ctermfg=white

set scrolloff=7             " Keep 7 lines above and below cursor
set wrap                    " Wrap lines
set wildmenu
set cmdheight=2
set incsearch               " incremental searching
set hlsearch                " Highlight matched queries
set lazyredraw              " Don't redraw when not needed
set showmatch               " Show matching brackets
set matchtime=2             " Blink matching parens .2 s for every second
set timeoutlen=500          " Wait 500 ms for key combinations to complete
set number                  " Show line number

" Todo: Make autocmd for terminal buffers to do :setlocal nonumber

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" Ignore some files
set wildignore=*.o,*~,*.pyc,*.class

" Sometimes ignore case when searching
set ignorecase
set smartcase

" ========== Status line ==========

set laststatus=2                    " Always show the status line

" Format the status line
set statusline=%f                           " relative file location
set statusline+=%m                          " modified flag
set statusline+=%r                          " ro flag
set statusline+=%h                          " help file flag
set statusline+=%w                          " preview flag
set statusline+=\ \ CWD:%{getcwd()}         " current working directory
set statusline+=\ [%l,%02.c]                " line and column number
set statusline+=[%02.p%%]                   " percent through file
set statusline+=%=%{dein#get_progress()}    " plugin update progress

" ========== Tabs and spaces ==========

" Use spaces instead of tabs
set expandtab
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set softtabstop=4
set tabstop=4

set autoindent

" ========== Tabs,windows,buffers ==========

set hidden      " Hide abandoned buffers

" Easy resizing
nnoremap <leader>A <C-w><
nnoremap <leader>a <C-w>>
nnoremap <leader>S <C-w>-
nnoremap <leader>s <C-w>+

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
" Terminal movement
tmap <C-j> <C-\><C-n><C-W>j
tmap <C-k> <C-\><C-n><C-W>k
tmap <C-h> <C-\><C-n><C-W>h
tmap <C-l> <C-\><C-n><C-W>l

" Close the current buffer but not the current window
map <leader>bd :bp \| bd #<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :+tabmove<cr>
map <leader>tM :-tabmove<cr>
map <leader>e :tabnext<cr>
map <leader>q :tabprev<cr>

" Opens a new split with the current buffer's path
map <leader>vs :vsplit <c-r>=expand("%:p:h")<cr>/<cr>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
set switchbuf=useopen,usetab,newtab
set stal=2

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" ========== Mappings ==========

" Make x go to blackhole buffer
nnoremap x "_x

" Unhighlight
nnoremap <leader><cr> :noh<cr>

" Easy escape
inoremap kj <Esc>
inoremap jk <Esc>
inoremap KJ <Esc>
inoremap JK <Esc>
tnoremap JK <C-\><C-n>
tnoremap KJ <C-\><C-n>

" Remap VIM 0 to first non-blank character
map 0 ^

" Move line(s) of text using ALT+[jk]
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Center when searching
map N Nzz
map n nzz

" Dont have to hold shift for commands
nore ; :

" Switch searching
nnoremap # *
nnoremap * #

