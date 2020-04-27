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

if (v:version >= 800 || has('nvim')) && isdirectory(expand('$HOME/.config/nvim/deind'))
    let g:dein_supported=1
endif


if g:dein_supported
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
        let g:coc_global_extensions = ['coc-json', 'coc-rust-analyzer']
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
        call dein#add('tpope/vim-commentary')

        " misc
        call dein#add('arcticicestudio/nord-vim')
        call dein#add('chrisbra/Colorizer')
        call dein#add('andymass/vim-tradewinds')
        call dein#add('airblade/vim-rooter')
        call dein#add('moll/vim-bbye')

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

if g:dein_supported
    " Colorizer
    nmap <Leader>cc <Plug>Colorizer
    xmap <Leader>cc <Plug>Colorizer

    " vim-rooter
    let g:rooter_manual_only = 1
    let g:rooter_patterns= [".git", ".git/", "makefile", "Makefile", "Cargo.toml"]

    " vim-bbye: Close the current buffer but not the current window
    nnoremap <silent> <leader>bd :Bdelete<cr>

    " coc.nvim_mappings {{{2
    " trigger completion
    inoremap <silent><expr> <c-space> coc#refresh()

    " Use `[c` and `]c` to navigate diagnostics
    nmap <silent> [c <Plug>(coc-diagnostic-prev)
    nmap <silent> ]c <Plug>(coc-diagnostic-next)

    " Rename current word
    nmap <leader>rn <Plug>(coc-rename)

    augroup CocConfig
        autocmd!

        " Highlight symbol under cursor on CursorHold
        autocmd CursorHold * silent call CocActionAsync('highlight')
    augroup END

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

" ========== general ========== {{{1

" general_settings {{{2
set history=1000
set fileformats=unix,dos,mac
set nrformats=bin,hex,alpha
set scrolloff=4
set cmdheight=2
set wrap
set hidden
set lazyredraw
set foldmethod=indent
set foldcolumn=1

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
command! -bar E edit $MYVIMRC
" Taken from: https://stackoverflow.com/a/3879737
function! SetupCommandAlias(from, to)
    exec 'cnoreabbrev <expr> '.a:from
          \ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:from.'")'
          \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfunction

call SetupCommandAlias('man', 'Man')
call SetupCommandAlias('vman', 'vertical Man')
call SetupCommandAlias('vMan', 'vertical Man')
call SetupCommandAlias('tman', 'tab Man')
call SetupCommandAlias('tMan', 'tab Man')
call SetupCommandAlias('vh', 'vertical help')
call SetupCommandAlias('sv', 'sview')
call SetupCommandAlias('vv', 'vertical sview')

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

        " Terminal behaves weird with scrolloff. Disable it for terminals
        autocmd TermEnter * let g:default_scrolloff=&scrolloff
                    \ | :set scrolloff=0
        autocmd TermLeave * let &scrolloff=g:default_scrolloff

        autocmd TermOpen * setlocal nonumber norelativenumber cursorline signcolumn=no

        " Automatically enter terminal-mode after opening
        autocmd TermOpen * startinsert

        " Search for the shell prompt
        function! GoToPrompt(flags) abort
            " Regexp representing my shell prompt
            let l:shell_prompt = '^\d\d\/\d\d|\d\d:\d\d\$'
            call search(shell_prompt, a:flags)
        endfunction

        " Jump to the shell prompt
        autocmd TermOpen * noremap <buffer> <silent> [g
                    \ :call GoToPrompt('eb')<cr>
        autocmd TermOpen * noremap <buffer> <silent> ]g
                    \ :call GoToPrompt('e')<cr>
        " Doesn't work in visual mode
        autocmd TermOpen * vunmap <buffer> [g
        autocmd TermOpen * vunmap <buffer> ]g

        autocmd TermOpen * nmap <buffer> [G 1G]g
        autocmd TermOpen * nmap <buffer> ]G G[g
        autocmd TermOpen * nmap <buffer> <C-c> a<C-c>

    augroup END
endif

augroup fugitive_autocommands
    autocmd!

    autocmd BufReadPost fugitive://* normal zR
augroup END

" ========== colorscheme ========== {{{1

augroup general-overrides
    autocmd!

    function! s:MatchTrailingWhitespace(isInsertMode)
        " Don't mess with highlighting for diffs
        if &filetype ==# 'fugitive'
            echom "Stop!"
            return
        endif
        if a:isInsertMode
            match ExtraWhitespace /\s\+\%#\@<!$/
        else
            match ExtraWhitespace /\s\+$/
        endif
    endfunction

    " Highlight trailing whitespaces
    autocmd ColorScheme * highlight ExtraWhitespace ctermbg=1 guibg=#BF616A

    " don't highlight trailing whitespace when typing at the end of a line.
    autocmd InsertEnter,VimEnter * call s:MatchTrailingWhitespace(1)
    autocmd InsertLeave * call s:MatchTrailingWhitespace(0)
augroup END

" Fix the absurdly low constrast of nord-vim
augroup nord-overrides
    autocmd!
    autocmd ColorScheme nord highlight Comment guifg=#7b88a1 gui=bold
    autocmd ColorScheme nord highlight Folded guifg=#7b88a1
    autocmd ColorScheme nord highlight FoldColumn guifg=#7b88a1
    autocmd ColorScheme nord highlight CocHighlightText guibg=#434C5E
augroup END

if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $TERM ==# 'xterm-kitty')
    set termguicolors
endif

if &termguicolors && g:dein_supported
    colorscheme nord
else
    colorscheme desert
endif

" ========== mappings ========== {{{1

" Move by wrapped lines unless a count is given
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')

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

    " Paste from a register
    tnoremap <expr> <C-V> '<C-\><C-N>"'.nr2char(getchar()).'pi'

endif

" Make, close, and move tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :+tabmove<cr>
nnoremap <leader>tM :-tabmove<cr>

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
" FIXME: Find out why these break count message when searching
"noremap N Nzz
"noremap n nzz

" Unhighlight
nnoremap <silent> <leader><cr> :nohlsearch<cr>

" Make x go to blackhole buffer
nnoremap x "_x

" More consistent with d
noremap Y y$

" Remap VIM 0 to first non-blank character
noremap 0 ^
"}}}
" vim:foldmethod=marker:foldlevel=1:
