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

function! Dein_supported()
    return (v:version >= 800 || has('nvim')) && isdirectory(expand('$HOME/.config/nvim/deind'))
endfunction

if Dein_supported()
    let g:dein#enable_notification = 1
    let g:dein#notification_time = 10
    let g:dein#notification_icon = "/usr/share/pixmaps/nvim.png"

    command! DeinInstall call dein#install()
    command! DeinUpdate call dein#update()

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
        let g:coc_global_extensions = ['coc-json', 'coc-python', 'coc-rust-analyzer']
        call dein#add('neoclide/coc.nvim', {
                    \ 'merged': 0,
                    \ 'rev': 'release',
                    \ })

        " Pairs
        call dein#add('jiangmiao/auto-pairs')
        call dein#add('machakann/vim-sandwich')
        call dein#add('junegunn/rainbow_parentheses.vim')

        " Language specific
        call dein#add('rust-lang/rust.vim')
        call dein#add('pest-parser/pest.vim')

        " tpope
        call dein#add('tpope/vim-unimpaired')
        call dein#add('tpope/vim-fugitive')

        " misc
        call dein#add('arcticicestudio/nord-vim')
        call dein#add('chrisbra/Colorizer')

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

let mapleader = ","

" coc.nvim_mappings {{{2
if Dein_supported() && dein#check_install("coc.nvim") == 0
    " trigger completion
    inoremap <silent><expr> <c-space> coc#refresh()

    " Use `[c` and `]c` to navigate diagnostics
    nmap <silent> [c <Plug>(coc-diagnostic-prev)
    nmap <silent> ]c <Plug>(coc-diagnostic-next)

    " Rename current word
    nmap <leader>rn <Plug>(coc-rename)

    " Highlight symbol under cursor on CursorHold
    autocmd CursorHold * silent call CocActionAsync('highlight')

    function! s:show_documentation()
        if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
        else
            call CocAction('doHover')
        endif
    endfunction

    " Use K to show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    " Go to definition
    nmap <silent> <space>g <Plug>(coc-definition)

    " Using CocList
    " Show all diagnostics
    nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
    " Find symbol of current document
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
    " Search workspace symbols
    nnoremap <silent> <space>s  :<C-u>CocList symbols<cr>
    " Do default action for next item.
    nnoremap <silent> <space>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
    " Resume latest coc list
    nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
endif

" colorizer mappings {{{2
if Dein_supported() && dein#check_install("Colorizer") == 0
    nmap <Leader>cc <Plug>Colorizer
    xmap <Leader>cc <Plug>Colorizer
endif

" ========== general ========== {{{1

" general_settings {{{2
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
" time to wait before saving file to disk
set updatetime=400
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

" general_autocmds {{{2
augroup general_autocommands
    " clear all autocmds
    autocmd!

    " Return to last edit position when opening files
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " Autoread is bugged. Force it to update buffer
    autocmd FocusGained,BufEnter * :silent! checktime
augroup END

if has('nvim')
    augroup terminal_autocommands
        autocmd!

        " Deal with glitchy terminal after clearing full screen
        autocmd TermEnter * setlocal scrolloff=0
        autocmd TermLeave * setlocal scrolloff=10

        autocmd TermOpen * setlocal nonumber norelativenumber cursorline signcolumn=no
        " Automatically enter terminal-mode after opening
        autocmd TermOpen * startinsert
        " Regexp representing my shell prompt
        let shell_prompt = '^\d\d\/\d\d|\d\d:\d\d\$'
        " Search for the shell prompt and then clear the last search
        " pattern since text highlighted in a term buffer is illegible
        " TODO: Instead, make a hl group for searching in term buffers
        " TODO: Make this work in visual mode too
        autocmd TermOpen * nnoremap <buffer> <silent> [g 
                    \ :silent! ?<C-r>=shell_prompt<cr><cr>
                    \ :let @/=""<cr>
                    \ :normal zt<cr>
        autocmd TermOpen * nnoremap <buffer> <silent> ]g
                    \ :silent! /<C-r>=shell_prompt<cr><cr>
                    \ :let @/=""<cr>
                    \ :normal zt<cr>
        autocmd TermOpen * nmap <buffer> [G 1G]g
        autocmd TermOpen * nmap <buffer> ]G GG[g
        autocmd TermOpen * nmap <buffer> <C-c> a<C-c>

    augroup END
endif

augroup fugitive_autocommands
    autocmd!

    autocmd BufReadPost fugitive://* normal zR
augroup END

" ========== colorscheme ========== {{{1

augroup gerneral-overrides
    autocmd!

    " Highlight trailing whitespaces
    autocmd ColorScheme * highlight ExtraWhitespace ctermbg=1 guibg=#BF616A

    " In insert mode, don't highlight trailing whitespace when typing at the
    " end of a line.
    autocmd InsertEnter,VimEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
augroup END

" Fix the absurdly low constrast of nord-vim
augroup nord-overrides
    autocmd!
    autocmd ColorScheme nord highlight Comment guifg=#7b88a1 gui=bold
    autocmd ColorScheme nord highlight Folded guifg=#7b88a1
    autocmd ColorScheme nord highlight FoldColumn guifg=#7b88a1
    autocmd ColorScheme nord highlight CocHighlightText guibg=#434C5E
augroup END

if Dein_supported() && dein#check_install("nord-vim") == 0
            \ && has('termguicolors') && $COLORTERM ==# 'truecolor'
    colorscheme nord
    set termguicolors
else
    colorscheme desert
endif

"match ExtraWhitespace /\s\+\%#\@<!$/

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
    nnoremap <leader>tv :vs +term<cr>
    nnoremap <leader>ts :sp +term<cr>
endif

" Make, close, and move tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :+tabmove<cr>
nnoremap <leader>tM :-tabmove<cr>

" Close the current buffer but not the current window
nnoremap <silent> <leader>bd :bp \| bd #<cr>
" Switch to alternate file
nnoremap <silent> <leader>bp :b #<cr>

" Switch CWD of current tab to the directory of the open buffer
noremap <leader>cd :tcd %:p:h<cr>:pwd<cr>

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
