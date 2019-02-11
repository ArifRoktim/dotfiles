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

" Set up virtual environment
if isdirectory(expand('$HOME/.local/venv3/bin'))
    if empty($VIRTUAL_ENV)
        echo "Virtual environment exists but isn't activated!"
    else
        let g:python3_host_prog = expand('$HOME/.local/venv3/bin/python3')
    endif
endif
if isdirectory(expand('$HOME/.local/venv2/bin'))
    let g:python_host_prog = expand('$HOME/.local/venv2/bin/python2')
endif

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
        " Autocomplete
        call dein#add('Shougo/deoplete.nvim')
        call dein#add('autozimu/LanguageClient-neovim', {
                    \ 'rev': 'next',
                    \ 'build': 'bash install.sh',
                    \ })

        " Pairs
        call dein#add('jiangmiao/auto-pairs')
        call dein#add('machakann/vim-sandwich')
        call dein#add('tpope/vim-unimpaired')

        " Misc
        call dein#add('arcticicestudio/nord-vim')
        call dein#add('tpope/vim-fugitive')

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

" ==========Plugin Settings ==========

let g:deoplete#enable_at_startup = 1

let g:LanguageClient_serverCommands = {
    \ 'python': [systemlist('which pyls')[0]],
    \ }

" Increasing comment contrast only supported with truecolor
if has('termguicolors') && $COLORTERM ==# 'truecolor'
    " Increase comment brightness by 20%
    let g:nord_comment_brightness = 20
endif

" ========== General ==========

let mapleader = ","

" Easily edit this file
command! E edit ~/.config/nvim/init.vim | tcd %:p:h | normal! zz

set history=500
set autoread
set encoding=utf8
set ffs=unix,dos,mac
set scrolloff=7
set wrap
set wildmenu
set wildignore=*.o,*.pyc,*.class
set cmdheight=1
set incsearch
set inccommand=nosplit
set hlsearch
set lazyredraw
set showmatch
set matchtime=2
set timeoutlen=500
set noshowmode
set number
set relativenumber
set autowrite
set ignorecase
set smartcase
set mouse=a
" Always show the tab line
set showtabline=2

" When splitting, put new window below/to the right of current window
set splitright
set splitbelow

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" Use spaces instead of tabs
" 1 tab == 4 spaces
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=4

" Default to system clipboard
if has('clipboard')
    set clipboard^=unnamedplus
endif

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

" :W sudo saves the file
" Bugged in neovim
if !has('nvim')
    command! W w !sudo tee % > /dev/null
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
augroup END
" ========== Colorscheme ==========

" Enable truecolor support if supported
if has('termguicolors') && $COLORTERM ==# 'truecolor'
    set termguicolors
endif
colorscheme nord

" Toggle highlighting current line
nnoremap <Leader>c :set cursorline!<cr>

" ========== Status line ==========

" Always show the status line
set laststatus=2

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
            \ 'r'  : 'Command',
            \ 'i'  : 'Insert',
            \ 'R'  : 'Replace',
            \ 't'  : 'Terminal'
            \}
" Colors for the modes
let g:modecolor={
            \ 'Normal'   : 'guifg=#D8DEE9 ctermfg=NONE',
            \ 'Command'  : 'guifg=#D8Dee9 ctermfg=NONE',
            \ 'Visual'   : 'guifg=#B48EAD ctermfg=5',
            \ 'Select'   : 'guifg=#B48EAD ctermfg=5',
            \ 'Insert'   : 'guifg=#88C0D0 ctermfg=6',
            \ 'Replace'  : 'guifg=#88C0D0 ctermfg=6',
            \ 'Terminal' : 'guifg=#A3BE8C ctermfg=2',
            \}

" Change color of statusline depending on mode
function! ChangeStatuslineColor() abort
    exec("highlight! User1 ".g:modecolor[g:currentmode[mode()]])
    return ''
endfunction

function! GetRelCWD() abort
    return substitute(getcwd(), $HOME, '~', '')
endfunction

" Format the status line
set statusline=
set statusline+=%1*                         " set highlight group to user1
set statusline+=%{ChangeStatuslineColor()}  " change color of statusline depending on mode
set statusline+=\ %{currentmode[mode()]}\   " current mode
set statusline+=%*                          " reset highlight group to default
set statusline+=%<                          " start truncating here when screen too narrow
set statusline+=\ CWD:%{GetRelCWD()}        " current working directory
set statusline+=\ %f                        " relative file location
set statusline+=%r                          " ro flag
set statusline+=%{&modified?'*':''}         " modified flag
set statusline+=\                           " Add space
set statusline+=%=                          " seperation point
set statusline+=[%l,%02.c]                  " line and column number
set statusline+=[%02.p%%]                   " percent through file

" ========== Tabs,windows,buffers ==========

" Hide abandoned buffers
set hidden

" Resizing
nnoremap <leader>A <C-w><
nnoremap <leader>D <C-w>>
nnoremap <leader>S <C-w>-
nnoremap <leader>W <C-w>+

" Make current window only window
nnoremap <leader>o <C-w><C-o>

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
nnoremap <leader>bd :bp \| bd #<cr>

" Useful noremappings for managing tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :+tabmove<cr>
nnoremap <leader>tM :-tabmove<cr>
if has('nvim')
    tnoremap <leader>tn <C-\><C-n>:tabnew<cr>
    tnoremap <leader>to <C-\><C-n>:tabonly<cr>
    tnoremap <leader>tc <C-\><C-n>:tabclose<cr>
    tnoremap <leader>tm <C-\><C-n>:+tabmove<cr>
    tnoremap <leader>tM <C-\><C-n>:-tabmove<cr>
    tnoremap <leader>e <C-\><C-n>:tabnext<cr>
    tnoremap <leader>q <C-\><C-n>:tabprev<cr>
endif

" Switch CWD of current tab to the directory of the open buffer
noremap <leader>cd :tcd %:p:h<cr>:pwd<cr>

" ========== Mappings ==========

" Make x go to blackhole buffer
nnoremap x "_x

" More consistent with d
noremap Y y$

" Unhighlight
nnoremap <leader><cr> :nohlsearch<cr>

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
