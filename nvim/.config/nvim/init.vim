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
"    => Helper functions
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
  "call dein#add('Shougo/deol.nvim')

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

"" Indent Line
"" Set color to sky blue
"let g:indentLine_color_term = 38 
"" Change indent character to pipe
"let g:indentLine_char = '|'
"
"" Rainbow parens 
"let g:rainbow_conf = {
"    \   'ctermfgs': ['blue', 'magenta', 'cyan', 'green'],
"    \   'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
"    \}
"" Enable by default
"let g:rainbow_active = 1

" ========== General ========== 

" Easily edit this file
command E edit ~/.config/nvim/init.vim

set gdefault                    " Use g flag for :s by default
set history=500                 " Sets how many lines of history VIM has to remember
set autoread                    " Set to auto read when a file is changed from the outside
let mapleader = ","

" :W sudo saves the file 
command W w !sudo tee % > /dev/null

set encoding=utf8               " Set utf8 as standard encoding and en_US as the standard language
set ffs=unix,dos,mac            " Use Unix as the standard file type
set nobackup                    " No backup files
"set noswapfile                 " No swap files
set clipboard^=unnamedplus      " Set default register to system clipboard

" ========== User Interface ========== 

colorscheme desert
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
set t_Co=256                " Enable 256 colors
set number                  " Show line number

" Todo: Make autocmd for terminal buffers to do :setlocal nonumber

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" Ignore some files
set wildignore=*.o,*~,*.pyc,*.class
set wildignore+=*/.git/*

" Sometimes ignore case when searching
set ignorecase
set smartcase

" ========== Status line ========== 

set laststatus=2                    " Always show the status line

" Format the status line
set statusline=%F                           "file location
set statusline+=%m                          "modified flag
set statusline+=%r                          "ro flag
set statusline+=%h                          "help file flag
set statusline+=%w                          "preview flag
set statusline+=\ \ CWD:%{getcwd()}         "current working directory
"set statusline+=\ [%l,%02.c%03.V]           "line and column number
set statusline+=\ [%l,%02.c]                "line and column number
set statusline+=[%02.p%%]                   "percent through file
set statusline+=%=%{dein#get_progress()}    "plugin update progress

" ========== Tabs and spaces ========== 

" Use spaces instead of tabs
set expandtab
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set softtabstop=4
set tabstop=4

set ai  "Auto indent"

" ========== Tabs,windows,buffers ========== 

set hidden      " Hide abandoned buffers

" Easy resizing
nnoremap <leader>A <C-w><
nnoremap <leader>a <C-w>>
nnoremap <leader>S <C-w>-
nnoremap <leader>s <C-w>+
" Resize from insert mode
inoremap <leader>A <esc><C-w><
inoremap <leader>a <esc><C-w>>
inoremap <leader>S <esc><C-w>+
inoremap <leader>s <esc><C-w>-

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
map <leader>bd :Bclose<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove<cr>
map <leader>t<leader> :tabnext<cr>
map <leader>T<leader> :tabprev<cr>

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Opens a new tab with the current buffer's path
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers 
set switchbuf=useopen,usetab,newtab
set stal=2

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" ========== Mappings ========== 

" Make x go to blackhole buffer
nnoremap x "_x

" Easy escape
inoremap kj <Esc>
inoremap jk <Esc>
inoremap KJ <Esc>
inoremap JK <Esc>
"tnoremap jk <C-\><C-n>
"tnoremap kj <C-\><C-n>
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

" Visual mode pressing # searches for the current selection
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>

" ========== Helper functions ========== 
function! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction 

function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", "\\/.*'$^~[]")
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'gv'
        call CmdLine("Ag '" . l:pattern . "' " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
   let l:currentBufNum = bufnr("%")
   let l:alternateBufNum = bufnr("#")

   if buflisted(l:alternateBufNum)
     buffer #
   else
     bnext
   endif

   if bufnr("%") == l:currentBufNum
     new
   endif

   if buflisted(l:currentBufNum)
     execute("bdelete! ".l:currentBufNum)
   endif
endfunction

