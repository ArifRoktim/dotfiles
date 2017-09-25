"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"    => General
"    => Plugin Settings
"    => Colorscheme
"    => Status line
"    => Tabs,windows,buffers
"    => Mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ========== dein Scripts ==========

" Required:
set runtimepath+=$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('$HOME/.config/nvim/deind')
  call dein#begin('$HOME/.config/nvim/deind')

  " Let dein manage dein
  " Required:
  call dein#add('$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:
  call dein#add('w0rp/ale')
  call dein#add('jiangmiao/auto-pairs')

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('Rip-Rip/clang_complete')
  call dein#add('zchee/deoplete-jedi')

  " You can specify revision/branch/tag.
  "call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
if !exists("g:syntax_on")
    syntax enable
endif

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

" ==========Plugin Settings ==========

" Lint only when files are saved
let g:ale_lint_on_text_changed = 'never'

" Lint python2 instead of python3
if hostname() == "iroh" || hostname() == "zuko"
    let g:ale_python_pylint_executable = 'pylint2'
else
    let g:ale_python_pylint_executable = 'pylint'
endif

" Enable deoplete
let g:deoplete#enable_at_startup = 1

let g:clang_complete_auto = 0
let g:clang_auto_select = 0
let g:clang_omnicppcomplete_compliance = 0
let g:clang_make_default_keymappings = 0

" ========== General ==========

let mapleader = ","

" Sets how many lines of history VIM has to remember
set history=500
" Set to auto read when a file is changed from the outside
set autoread

" Easily edit this file
command E edit ~/.config/nvim/init.vim

" :W sudo saves the file
command W w !sudo tee % > /dev/null

set encoding=utf8                   " Set utf8 as standard encoding
set ffs=unix,dos,mac                " Use correct EOL format
set clipboard^=unnamedplus          " Set default register to system clipboard
set scrolloff=7                     " Keep 7 lines above and below cursor
set wrap                            " Wrap lines
set wildmenu                        " Enable wildmenu
set wildignore=*.o,*.pyc,*.class    " Ignore some files
set cmdheight=2                     " Leave space at the bottom
set incsearch                       " incremental searching
set hlsearch                        " Highlight matched queries
set lazyredraw                      " Don't redraw when not needed
set showmatch                       " Show matching brackets
set matchtime=2                     " Blink matching parens .2 s for every second
set timeoutlen=500                  " Wait 500 ms for key combinations to complete
set number
set relativenumber
set autowrite                       " Write the contents of the file, if it has been modified on certain commands
" Sometimes ignore case when searching
set ignorecase
set smartcase

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" Use spaces instead of tabs
set expandtab
set smarttab
" 1 tab == 4 spaces
set shiftwidth=4
set softtabstop=4

" Persistent undo
if has("persistent_undo")
    set undodir=~/.config/nvim/_undo
    set undofile
    set backup
    set backupdir=~/.config/nvim/_tmp
    set dir=~/.config/nvim/_swap
endif

set autoindent
augroup Autocmds
    " clear all autocmds
    autocmd!

    " make sure terminal buffers don't have line numbers
    autocmd BufEnter term://* setlocal nonumber | setlocal norelativenumber

    " Return to last edit position when opening files
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " Autoread is bugged. Force it to update buffer
    autocmd FocusGained,BufEnter * :silent! checktime

    " Close preview windows after autocomplete
    "autocmd CompleteDone * silent! pclose!
augroup END
" ========== Colorscheme ==========


augroup colorschm
    autocmd!
    autocmd ColorScheme * highlight clear SignColumn | highlight CursorLine cterm=NONE ctermbg=darkblue | highlight Pmenu ctermbg=darkcyan ctermfg=black | highlight PmenuSel ctermbg=blue ctermfg=white | highlight ALEErrorSign ctermbg=darkcyan ctermfg=black | highlight ALEWarningSign ctermbg=blue ctermfg=black
augroup END

colorscheme desert

" Toggle highlighting current line
nnoremap <Leader>c :set cursorline!<cr>

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
tnoremap <C-j> <C-\><C-n><C-W>j
tnoremap <C-k> <C-\><C-n><C-W>k
tnoremap <C-h> <C-\><C-n><C-W>h
tnoremap <C-l> <C-\><C-n><C-W>l

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
noremap 0 ^

" Move line(s) of text using ALT+[jk]
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Center when searching
noremap N Nzz
noremap n nzz

