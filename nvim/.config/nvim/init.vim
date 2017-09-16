"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"    => General
"    => Colorscheme
"    => Status line
"    => Tabs,windows,buffers
"    => Mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ========== General ==========

set nocompatible
filetype plugin indent on
syntax enable

" Easily edit this file
command E edit ~/.config/nvim/init.vim

let mapleader = ","

" Sets how many lines of history VIM has to remember
set history=500
" Set to auto read when a file is changed from the outside
set autoread
" Autoread is bugged. Force it to update buffer
au FocusGained,BufEnter * :silent! checktime

" :W sudo saves the file
command W w !sudo tee % > /dev/null

set encoding=utf8               " Set utf8 as standard encoding
set ffs=unix,dos,mac            " Use correct EOL format
set nobackup                    " No backup files
set clipboard^=unnamedplus      " Set default register to system clipboard
set scrolloff=7                 " Keep 7 lines above and below cursor
set wrap                        " Wrap lines
set wildmenu                    " Enable wildmenu
" Ignore some files
set wildignore=*.o,*~,*.pyc,*.class
set cmdheight=2
set incsearch                   " incremental searching
set hlsearch                    " Highlight matched queries
set lazyredraw                  " Don't redraw when not needed
set showmatch                   " Show matching brackets
set matchtime=2                 " Blink matching parens .2 s for every second
set timeoutlen=500              " Wait 500 ms for key combinations to complete
set number
set relativenumber
" Sometimes ignore case when searching
set ignorecase
set smartcase

" make sure terminal buffers don't have line numbers
autocmd BufEnter term://* setlocal nonumber | setlocal norelativenumber

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" Use spaces instead of tabs
set expandtab
set smarttab
" 1 tab == 4 spaces
set shiftwidth=4
set softtabstop=4
set tabstop=4

set autoindent

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" ========== Colorscheme ==========

colorscheme desert
highlight clear SignColumn

" Toggle highlighting current line
hi CursorLine cterm=NONE ctermbg=darkblue
nnoremap <Leader>c :set cursorline!<cr>

" Change color of popup menu
hi Pmenu    ctermbg=darkcyan ctermfg=black
hi PmenuSel ctermbg=blue ctermfg=white

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
noremap <leader>bd :bp \| bd #<cr>

noremap <leader>l :bnext<cr>
noremap <leader>h :bprevious<cr>

" Useful noremappings for managing tabs
noremap <leader>tn :tabnew<cr>
noremap <leader>to :tabonly<cr>
noremap <leader>tc :tabclose<cr>
noremap <leader>tm :+tabmove<cr>
noremap <leader>tM :-tabmove<cr>
noremap <leader>e :tabnext<cr>
noremap <leader>q :tabprev<cr>

" Opens a new split with the current buffer's path
noremap <leader>vs :vsplit <c-r>=expand("%:p:h")<cr>/<cr>

" Switch CWD to the directory of the open buffer
noremap <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
set switchbuf=useopen,usetab,newtab
set stal=2

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
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Center when searching
map N Nzz
map n nzz

