"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"    => virtualenv
"    => dein scripts
"    => plugin settings
"    => general
"    => colorscheme
"    => status line
"    => mappings
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" ========== virtualenv ========== {{{1
if !empty($VIRTUAL_ENV) && isdirectory(expand('$HOME/.local/venv3/bin'))
    let g:python3_host_prog = expand('$HOME/.local/venv3/bin/python3')
endif
if isdirectory(expand('$HOME/.local/venv2/bin'))
    let g:python_host_prog = expand('$HOME/.local/venv2/bin/python2')
endif

" ========== dein scripts ========== {{{1

if has('nvim') && isdirectory(expand('$HOME/.config/nvim/deind'))
    " Required:
    set runtimepath+=$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim

    " Required:
    if dein#load_state('$HOME/.config/nvim/deind')
        call dein#begin('$HOME/.config/nvim/deind')

        " Let dein manage dein
        " Required:
        call dein#add('$HOME/.config/nvim/deind/repos/github.com/Shougo/dein.vim')

        " Add or remove your plugins here:{{{2
        " Autocomplete
        call dein#add('Shougo/deoplete.nvim')
        call dein#add('autozimu/LanguageClient-neovim', {
                    \ 'rev': 'next',
                    \ 'build': 'bash install.sh',
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

        " Misc
        call dein#add('arcticicestudio/nord-vim', {
                    \ 'hook_add': "
                    \ if has('termguicolors') && $COLORTERM ==# 'truecolor'\n
                    \     set termguicolors\n
                    \     let g:nord_comment_brightness = 20\n
                    \ endif
                    \ "
                    \ })
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

" ========== plugin settings ========== {{{1

let g:deoplete#enable_at_startup = 1

let s:pyls = system('which pyls')
if !v:shell_error
    let g:LanguageClient_serverCommands = {
        \ 'python': [systemlist('which pyls')[0]],
        \ }
endif

" ========== general ========== {{{1

let mapleader = ","

" sets {{{2
" misc
set history=1000
set fileformats=unix,dos,mac
set nrformats=bin,hex,octal,alpha
set scrolloff=7
set cmdheight=1
set wrap
set lazyredraw
set timeoutlen=1000
set mouse=a
set foldmethod=indent
set hidden

set number
set relativenumber

set autoread
set autowrite

" split new window below or to the right of current window
set splitright
set splitbelow

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

" commands {{{2
" :W sudo saves the file
" Bugged in neovim
if !has('nvim')
    command! W w !sudo tee % > /dev/null
endif

" Easily edit this file
command! -bar E edit $MYVIMRC | tcd %:p:h | normal! zz

" Make a new scratch buffer
command! -bar Newscratch <mods> new +set\ buftype=nofile
command! -bar Vnewscratch vertical Newscratch

" Read command into a scratch buffer
command! -nargs=1 -complete=command IntoScratch <mods> Newscratch | silent put!=execute(\"<args>\")
command! -nargs=1 -complete=command VintoScratch <mods> vertical IntoScratch <args>

" I look at the messages pretty often
command! Lookmessages <mods> IntoScratch messages
command! Vlookmessages <mods> vertical Lookmessages

" autocmds {{{2
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



" ========== colorscheme ========== {{{1

augroup colorscheme_autocommands
    autocmd ColorScheme * highlight User1 guifg=#D8DEE9
    autocmd ColorScheme * highlight User2 guifg=#D8DEE9 guibg=#3B4252
augroup END
colorscheme nord

" ========== status line ========== {{{1

" Modes dictionary {{{2
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

" functions used in statusline {{{2
" Change color of statusline depending on mode
function! ChangeStatuslineColor() abort
    execute("highlight! User1 ".g:modecolor[g:currentmode[mode()]])
    return ''
endfunction

function! StripGit() abort
    " Turns [Git(master)] -> (master)
    " and   [Git:0123456(master)] -> 0123456(master)
    return substitute(FugitiveStatusline(),
                \ '\[Git\%[:]\(.*\)(\(.\{-\}\))\]', '\1(\2 ', "")
endfunction

function! GetCwdAndFile() abort
    " Returns the cwd and/or the buffer number and filename
    "
    " Changes cwd by replacing $HOME with ~ and 
    " stripping the sha from fugitive buffer names.
    " Replaces $HOME with ~ in % and prepends the buffer number

    function! ReplaceHome(input) abort
        return substitute(a:input, $HOME, '~', '')
    endfunction
    let s:cwd=ReplaceHome(getcwd(-1, 0))
    let s:file=ReplaceHome(expand('%'))
    let s:bufnr=bufnr('%') . ":"

    if expand('%') =~# '^fugitive://'
        " File is a fugitive object so return just the file path
        " Remove the sha because it's too long.
        return ['', s:bufnr, substitute(s:file, 
                    \ '^fugitive://\(.\{-\}\).git//.\{40}', 'fugitive://\1', "")]
    elseif expand('%') =~# '^$'
        " No file name
        return ['', '', 'No Name']
    elseif expand('%') =~# '^/' || expand('%') =~# '\w\+://'
        " File name is absolute or isn't on the filesystem
        " Examples: /home/<blah...> or fugitive://~/dotfiles/.git/...
        " Don't print cwd. Just print buffer number and file path
        return ['', s:bufnr, s:file]
    else
        " Print cwd and file
        return [s:cwd, s:bufnr, s:file]
    endif
endfunction

"}}}2
" Format the status line
set statusline=
set statusline+=%1*                         " set highlight group to user1
set statusline+=%{ChangeStatuslineColor()}  " change color of statusline depending on mode
set statusline+=\ %{currentmode[mode()]}\   " current mode
set statusline+=%<                          " start truncating here when screen too narrow
set statusline+=%2*                         " set highlight group to user2
" if GetCwdAndFile()[0] is not empty, display a space and then it
set statusline+=%{len(GetCwdAndFile()[0])?'\ '.GetCwdAndFile()[0]:''}
set statusline+=\ %{StripGit()}             " Show branch and file's commit
set statusline+=%*\                         " reset highlight group to default
set statusline+=%{GetCwdAndFile()[1]}       " buffer number if buffer has name
set statusline+=%{GetCwdAndFile()[2]}
set statusline+=%r                          " ro flag
set statusline+=%{&modified?'*':''}         " modified flag
set statusline+=\                           " Add space
set statusline+=%=                          " seperation point
set statusline+=[%l,%02.c]                  " line and column number
set statusline+=[%02.p%%]                   " percent through file

" ========== mappings ========== {{{1

" Tabs, windows, and buffers {{{2
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
