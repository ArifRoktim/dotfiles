"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections: Jump with * or #
"    => virtualenv
"    => dein_scripts
"       => dein_plugins
"    => plugin_settings
"    => general
"       => general_settings
"       => general_commands
"       => general_autocmds
"    => colorscheme
"    => mappings
"       => tabs_windows_buffers
"       => coc.nvim_mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ========== virtualenv ========== {{{1
if !empty($VIRTUAL_ENV) && isdirectory(expand('$HOME/.local/venv3/bin'))
    let g:python3_host_prog = expand('$HOME/.local/venv3/bin/python3')
endif
if isdirectory(expand('$HOME/.local/venv2/bin'))
    let g:python_host_prog = expand('$HOME/.local/venv2/bin/python2')
endif

" ========== dein_scripts ========== {{{1

if has('nvim') && isdirectory(expand('$HOME/.config/nvim/deind'))
    " Required:
    set runtimepath+=$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim

    " Required:
    if dein#load_state('$HOME/.config/nvim/deind')
        call dein#begin('$HOME/.config/nvim/deind')

        " Let dein manage dein
        " Required:
        call dein#add('$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim')

        " dein_plugins:{{{2
        " Autocomplete
        call dein#add('neoclide/coc.nvim', {
                    \ 'rev': '*',
                    \ 'build': 'yarnpkg install',
                    \ })

        " Pairs
        call dein#add('jiangmiao/auto-pairs')
        call dein#add('machakann/vim-sandwich')

        " Language specific
        call dein#add('rust-lang/rust.vim')

        " tpope
        call dein#add('tpope/vim-unimpaired')
        call dein#add('tpope/vim-fugitive')
        call dein#add('tpope/vim-commentary')

        " Colorscheme
        if has('termguicolors') && $COLORTERM ==# 'truecolor'
            set termguicolors
        endif
        call dein#add('ArifRoktim/nord-vim')

        "}}}2
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

" ========== plugin_settings ========== {{{1

" ========== general ========== {{{1

let mapleader = ","

" general_settings {{{2
" misc
set history=1000
set fileformats=unix,dos,mac
set nrformats=bin,hex,alpha
set scrolloff=7
set cmdheight=2
set wrap
set hidden
set lazyredraw
set foldmethod=indent

set number
set relativenumber

set autoread
set autowrite

" always show signcolumn
set signcolumn=yes
" don't give ins-completion-menu messages
set shortmess+=c
" time in ms to wait for mapped sequence to complete
set timeoutlen=1000
" enable mouse support most everything
set mouse=a

" split new window below or to the right of current window
set splitright
set splitbelow
" dont resize when closing a split
set noequalalways

" Flash matching parens for 2/10 seconds
set showmatch
set matchtime=2

" Always show the tab and status line
set showtabline=2
set laststatus=2
set noshowmode

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" wildmenu settings
set wildmenu
" complete till longest common string and list all matches,
" then complete next full match
set wildmode=list:longest,full
set wildignore=*.o,*.pyc,*.class
" enable tab completion from cnoremap
set wildcharm=<tab>

" Search/replace settings
set incsearch
set inccommand=nosplit
set hlsearch
set nowrapscan
set ignorecase
set smartcase

" Space/Tab settings
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

" general_commands {{{2
" :W sudo saves the file
" Bugged in neovim
if !has('nvim')
    command! W w !sudo tee % > /dev/null
endif

" Easily edit this file
command! -bar E edit $MYVIMRC | tcd %:p:h | normal! zz

command! Echofilename echo expand('%')

" Make a new scratch buffer
command! -bar Newscratch <mods> new +set\ buftype=nofile
command! -bar Vnewscratch vertical Newscratch

" Read command into a scratch buffer
command! -nargs=1 -complete=command IntoScratch <mods> Newscratch | silent put!=execute(\"<args>\")
command! -nargs=1 -complete=command VintoScratch <mods> vertical IntoScratch <args>

" I look at the messages pretty often
command! Lookmessages <mods> IntoScratch messages
command! Vlookmessages <mods> vertical Lookmessages

" general_autocmds {{{2
augroup general_autocommands
    " clear all autocmds
    autocmd!

    " Return to last edit position when opening files
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " Autoread is bugged. Force it to update buffer
    autocmd FocusGained,BufEnter * :silent! checktime
augroup END

augroup terminal_autocommands
    autocmd!

    if has('nvim')
        " make sure terminal buffers don't have line numbers
        autocmd TermOpen * setlocal nonumber norelativenumber cursorline
        " Automatically enter terminal-mode after opening
        autocmd TermOpen * startinsert
        " Regexp representing my shell prompt
        let shell_prompt = '^\d\d\/\d\d|\d\d:\d\d\$ '
        " Search for the shell prompt and then clear the last search
        " pattern since text highlighted in a term buffer is illegible
        " TODO: Instead, make a hl group for searching in term buffers
        autocmd TermOpen * nnoremap <buffer> <silent> [g 
                    \ :silent! ?<C-r>=shell_prompt<cr><cr>
                    \ :let @/=""<cr>
        autocmd TermOpen * nnoremap <buffer> <silent> ]g
                    \ :silent! /<C-r>=shell_prompt<cr><cr>
                    \ :let @/=""<cr>
        autocmd TermOpen * nmap <buffer> [G 1G]g
        autocmd TermOpen * nmap <buffer> ]G GG[g
    endif

augroup END

augroup fugitive_autocommands
    autocmd!

    autocmd BufReadPost fugitive://* normal zR
augroup END

" ========== colorscheme ========== {{{1

colorscheme nord

" ========== mappings ========== {{{1

" tabs_windows_buffers {{{2
" Move between windows
nnoremap <leader>s <C-W>j
nnoremap <leader>w <C-W>k
nnoremap <leader>a <C-W>h
nnoremap <leader>d <C-W>l
if has('nvim')
    " Terminal movement
    tnoremap <leader>s <C-\><C-n><C-W>j
    tnoremap <leader>w <C-\><C-n><C-W>k
    tnoremap <leader>a <C-\><C-n><C-W>h
    tnoremap <leader>d <C-\><C-n><C-W>l
endif

" Make, close, and move tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :+tabmove<cr>
nnoremap <leader>tM :-tabmove<cr>

" Close the current buffer but not the current window
nnoremap <leader>bd :bp \| bd #<cr>

" Switch CWD of current tab to the directory of the open buffer
noremap <leader>cd :tcd %:p:h<cr>:pwd<cr>

" coc.nvim_mappings {{{2
inoremap <silent><expr> <c-space> coc#refresh()

"}}}2
" Easy escape
inoremap kj <Esc>
inoremap jk <Esc>
inoremap KJ <Esc>
inoremap JK <Esc>
if has('nvim')
    tnoremap JK <C-\><C-n>
    tnoremap KJ <C-\><C-n>
endif

" Edit a file with the directory of current file pre-populated
nnoremap <leader>e :e <C-r>=expand('%:h')<CR>/
cnoremap <leader>e <C-r>=expand('%:h')<CR>/

" Center when searching
noremap N Nzz
noremap n nzz

" Unhighlight
nnoremap <leader><cr> :nohlsearch<cr>

" Make x go to blackhole buffer
nnoremap x "_x

" More consistent with d
noremap Y y$

" Remap VIM 0 to first non-blank character
noremap 0 ^
"}}}
" vim:foldmethod=marker
