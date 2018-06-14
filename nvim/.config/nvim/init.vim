"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"    => Plugin Settings
"    => General
"    => Colorscheme
"    => Status line
"    => Tabs,windows,buffers
"    => Mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ========== dein Scripts ==========

if has('nvim') && isdirectory(expand('$HOME/.config/nvim/deind'))
    " Required:
    set runtimepath+=$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim

    " Required:
    if dein#load_state('$HOME/.config/nvim/deind')
        call dein#begin('$HOME/.config/nvim/deind')

        " Let dein manage dein
        " Required:
        call dein#add('$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim')

        " Add or remove your plugins here:
        call dein#add('Shougo/deoplete.nvim')
        call dein#add('jiangmiao/auto-pairs')
        call dein#add('tpope/vim-repeat')

        call dein#add('rust-lang/rust.vim')
        call dein#add('racer-rust/vim-racer')

        " You can specify revision/branch/tag.
        "call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

        " Required:
        call dein#end()
        call dein#save_state()
    endif

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

" Enable deoplete
let g:deoplete#enable_at_startup = 1

" Configure racer
let g:racer_cmd = "$HOME/.cargo/bin/racer"
let g:racer_experimental_completer = 1

" ========== General ==========

let mapleader = ","

" Sets how many lines of history VIM has to remember
set history=500
" Set to auto read when a file is changed from the outside
set autoread

" Easily edit this file
command! E edit ~/.config/nvim/init.vim

" :W sudo saves the file
command! W w !sudo tee % > /dev/null

if has('clipboard')
    set clipboard^=unnamedplus      " Set default register to system clipboard
endif
set encoding=utf8                   " Set utf8 as standard encoding
set ffs=unix,dos,mac                " Use correct EOL format
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
    if !isdirectory("~/.config/nvim/_tmp")
        silent! call mkdir( $HOME.'/.config/nvim/_tmp', "p")
    endif
endif

augroup Autocmds
    " clear all autocmds
    autocmd!

    if has('nvim')
        " make sure terminal buffers don't have line numbers
        autocmd TermOpen * setlocal nonumber norelativenumber
    endif

    " Return to last edit position when opening files
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " Autoread is bugged. Force it to update buffer
    autocmd FocusGained,BufEnter * :silent! checktime

    " Close preview windows after autocomplete
    "autocmd CompleteDone * silent! pclose!
augroup END
" ========== Colorscheme ==========

if has('termguicolors') && $TERM == "st-256color"
    set termguicolors
endif

augroup colorschm
    autocmd!
    autocmd ColorScheme * highlight clear SignColumn
    autocmd ColorScheme * highlight CursorLine gui=NONE guibg='#04252d' cterm=NONE ctermbg=darkblue
    autocmd ColorScheme * highlight Pmenu guibg=cyan guifg=black ctermbg=cyan
    autocmd ColorScheme * highlight PmenuSel guibg=blue guifg=white ctermbg=blue
    autocmd ColorScheme * highlight ALEErrorSign guibg=cyan guifg=black ctermbg=cyan
    autocmd ColorScheme * highlight ALEWarningSign guibg=lightblue guifg=black ctermbg=lightblue
    autocmd ColorScheme * highlight Normal guibg='#073642'
    autocmd ColorScheme * highlight NonText guibg='#073642'
    autocmd Colorscheme * highlight StatusLine guibg='#a6a6a6'ctermfg=gray ctermbg=black
    autocmd Colorscheme * highlight User1 guibg=white guifg=black ctermbg=white ctermfg=black
augroup END

colorscheme desert

" Toggle highlighting current line
nnoremap <Leader>c :set cursorline!<cr>

" ========== Status line ==========

set laststatus=2                    " Always show the status line

" Modes
let g:currentmode={
            \ 'n'  : 'Normal',
            \ 'v'  : 'Visual',
            \ 'V'  : 'Visual',
            \ '' : 'Visual',
            \ 's'  : 'Select',
            \ 'S'  : 'Select',
            \ '' : 'Select',
            \ 'c'  : 'Command',
            \ 'i'  : 'Insert',
            \ 'r'  : 'Replace',
            \ 'R'  : 'Replace',
            \ 't'  : 'Terminal'
            \}

" Change color of statusline depending on mode
function! ChangeStatuslineColor() abort
    if (mode() ==# 'n')
        highlight! StatusLine guibg=#a6a6a6 ctermfg=gray
    elseif (g:currentmode[mode()] ==# 'Visual' || g:currentmode[mode()] ==# 'Select')
        highlight! StatusLine guibg=#ff66ff ctermfg=magenta
    elseif (mode() ==# 'i')
        highlight! StatusLine guibg=#7fbfff ctermfg=lightblue
    elseif (mode() ==# 't')
        highlight! StatusLine guibg=#7fff7f ctermfg=lightgreen
    endif
    return ''
endfunction

" Format the status line
set statusline=
set statusline+=%{ChangeStatuslineColor()}  " change color of statusline depending on mode
set statusline+=\ %{currentmode[mode()]}    " current mode
set statusline+=\ %1*                       " set highlight group to user1
set statusline+=\ %f                        " relative file location
set statusline+=%m                          " modified flag
set statusline+=%r                          " ro flag
set statusline+=%h                          " help file flag
set statusline+=%w                          " preview flag
set statusline+=\ %<
set statusline+=\ \ CWD:%{getcwd()}         " current working directory
set statusline+=\ [%l,%02.c]                " line and column number
set statusline+=[%02.p%%]                   " percent through file

" ========== Tabs,windows,buffers ==========

set hidden      " Hide abandoned buffers

" Easy resizing
nnoremap <leader>A <C-w><
nnoremap <leader>D <C-w>>
nnoremap <leader>S <C-w>-
nnoremap <leader>W <C-w>+

" Move between windows
nnoremap <leader>s <C-W>j
nnoremap <leader>w <C-W>k
nnoremap <leader>a <C-W>h
nnoremap <leader>d <C-W>l
inoremap <leader>s <Esc><C-W>j
inoremap <leader>w <Esc><C-W>k
inoremap <leader>a <Esc><C-W>h
inoremap <leader>d <Esc><C-W>l
if has('nvim')
    " Terminal movement
    tnoremap <leader>s <C-\><C-n><C-W>j
    tnoremap <leader>w <C-\><C-n><C-W>k
    tnoremap <leader>a <C-\><C-n><C-W>h
    tnoremap <leader>d <C-\><C-n><C-W>l
endif

"TODO: Make terminal mapping to jump between the prompts

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
if has('nvim')
    tnoremap <leader>tn <C-\><C-n>:tabnew<cr>
    tnoremap <leader>to <C-\><C-n>:tabonly<cr>
    tnoremap <leader>tc <C-\><C-n>:tabclose<cr>
    tnoremap <leader>tm <C-\><C-n>:+tabmove<cr>
    tnoremap <leader>tM <C-\><C-n>:-tabmove<cr>
    tnoremap <leader>e <C-\><C-n>:tabnext<cr>
    tnoremap <leader>q <C-\><C-n>:tabprev<cr>
endif
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

" Don't wanna use these keys
map <C-b> <Nop>
map <C-n> <Nop>

" More consistent with d
noremap Y y$

" Unhighlight
nnoremap <leader><cr> :noh<cr>

" Easy escape
inoremap kj <Esc>
inoremap jk <Esc>
inoremap KJ <Esc>
inoremap JK <Esc>
if has('nvim')
    tnoremap JK <C-\><C-n>
    tnoremap KJ <C-\><C-n>
endif

" Remap VIM 0 to first non-blank character
noremap 0 ^

" Center when searching
noremap N Nzz
noremap n nzz

