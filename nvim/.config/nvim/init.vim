"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections: Jump with * or #
"    => virtualenv
"    => dein_scripts
"       => dein_plugins
"    => plugin_settings
"       => colorscheme_settings
"    => general_settings
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

if has('nvim') && isdirectory(expand('$HOME/.config/nvim/deind'))
    let g:dein_supported=1
else
    let g:dein_supported=0
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

        " dein_plugins {{{2
        " Autocomplete
        let g:coc_global_extensions = ['coc-json', 'coc-rust-analyzer']
        call dein#add('neoclide/coc.nvim', {
                    \ 'merged': 0,
                    \ 'rev': 'release',
                    \ })

        " Pairs
        call dein#add('jiangmiao/auto-pairs')
        call dein#add('machakann/vim-sandwich')

        " Language specific
        call dein#add('rust-lang/rust.vim')
        call dein#add('pest-parser/pest.vim')

        " tpope
        call dein#add('tpope/vim-unimpaired')
        call dein#add('tpope/vim-fugitive')
        call dein#add('tpope/vim-commentary')

        " misc
        call dein#add('arcticicestudio/nord-vim')
        call dein#add('andymass/vim-tradewinds')
        call dein#add('moll/vim-bbye')
        call dein#add('aymericbeaumet/vim-symlink')
        call dein#add('airblade/vim-rooter')
        call dein#add('norcalli/nvim-colorizer.lua')
        call dein#add('Yggdroot/indentLine')

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
" Use ,, as ,
nnoremap <leader><leader> <leader>

if g:dein_supported
    " nvim-colorizer.lua
    if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $TERM ==# 'xterm-kitty')
        set termguicolors
        lua require 'colorizer'.setup(nil, { css = true; } )
        nnoremap <silent> <Leader>cc :ColorizerToggle<cr>
    endif

    " vim-rooter
    let g:rooter_manual_only = 1
    let g:rooter_cd_cmd = 'tcd'
    let g:rooter_patterns= [".git", "makefile", "Makefile", "Cargo.lock"]

    " vim-bbye: Close the current buffer but not the current window
    nnoremap <silent> <leader>bd :Bdelete<cr>

    " Disable some autopair bindings
    let g:AutoPairsShortcutToggle = ""
    let g:AutoPairsShortcutJump = ""
    let g:AutoPairsShortcutBackInsert = ""

    " indentLine
    let g:indentLine_setConceal = 0
    set conceallevel=1
    let g:indentLine_setColors = 0
    let g:indentLine_char = "¦"

    " coc.nvim_mappings {{{2
    augroup CocConfig
        autocmd!
        " Highlight symbol under cursor on CursorHold
        autocmd CursorHold * silent call CocActionAsync('highlight')
    augroup END

    " Use K to show documentation in preview window
    function! s:show_documentation()
        if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
        elseif (coc#rpc#ready())
            call CocActionAsync('doHover')
        else
            execute '!' . &keywordprg . " " . expand('<cword>')
        endif
    endfunction
    nnoremap <silent> K :call <SID>show_documentation()<CR>
    nnoremap <silent> <C-k> :<C-U>CocCommand rust-analyzer.openDocs<CR>

    " Insert mode floating window scrolling {{{
    " From: https://github.com/neoclide/coc.nvim/issues/1405#issuecomment-569984736
    function! s:coc_float_scroll(forward) abort
        let float = coc#util#get_float()
        if !float | return '' | endif
        let buf = nvim_win_get_buf(float)
        let buf_height = nvim_buf_line_count(buf)
        let win_height = nvim_win_get_height(float)
        if buf_height < win_height | return '' | endif
        let pos = nvim_win_get_cursor(float)
        if a:forward
            if pos[0] == 1
                let pos[0] += 3 * win_height / 4
            elseif pos[0] + win_height / 3 + 1 < buf_height
                let pos[0] += win_height / 3 + 1
            else
                let pos[0] = buf_height
            endif
        else
            if pos[0] == buf_height
                let pos[0] -= 3 * win_height / 4
            elseif pos[0] - (win_height / 3 + 1) > 1
                let pos[0] -= win_height / 3 + 1
            else
                let pos[0] = 1
            endif
        endif
        call nvim_win_set_cursor(float, pos)
        return ''
    endfunction
    " }}}
    inoremap <silent><expr> <down> coc#util#has_float() ? <SID>coc_float_scroll(1) : "\<down>"
    inoremap <silent><expr> <up> coc#util#has_float() ? <SID>coc_float_scroll(0) : "\<up>"

    " trigger completion
    inoremap <silent><expr> <C-space> coc#refresh()

    " Rename current word
    nmap <M-r> <Plug>(coc-rename)

    " Go to definition
    nmap <silent> <M-d> <Plug>(coc-definition)
    nmap <silent> <M-i> <Plug>(coc-implementation)

    " CocAction
    nmap <silent> <M-a> <Plug>(coc-codeaction-selected)
    vmap <silent> <M-a> <Plug>(coc-codeaction-selected)
    nmap <silent> <M-q> <Plug>(coc-codelens-action)

    " Using CocList
    " Show all diagnostics
    nnoremap <silent> <M-w> :<C-u>CocList diagnostics<cr>
    " Navigate diagnostics
    nmap <silent> [w <Plug>(coc-diagnostic-prev)
    nmap <silent> ]w <Plug>(coc-diagnostic-next)
    " Find symbol of current file
    nnoremap <silent> <M-o> :<C-u>CocList outline<cr>
    " Search workspace symbols
    nnoremap <silent> <M-s> :<C-u>CocList symbols<cr>
    " Do default action for next item.
    nnoremap <silent> <M-j> :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent> <M-k> :<C-u>CocPrev<CR>
    " Resume latest coc list
    nnoremap <silent> <M-l> :<C-u>CocListResume<CR>

    " colorscheme_settings {{{2

    augroup trailing_whitespace
        autocmd!
        function! s:MatchTrailingWhitespace(isInsertMode)
            " Don't mess with highlighting for fugitive
            if &filetype =~# 'git'
                return
            endif
            if a:isInsertMode
                match ExtraWhitespace /\s\+\%#\@<!$/
            else
                match ExtraWhitespace /\s\+$/
            endif
        endfunction

        " Highlight trailing whitespaces
        autocmd ColorScheme * highlight ExtraWhitespace ctermbg=1 gui=undercurl guisp=#BF616A

        " don't highlight trailing whitespace when typing at the end of a line.
        autocmd InsertEnter * call s:MatchTrailingWhitespace(1)
        autocmd InsertLeave,VimEnter * call s:MatchTrailingWhitespace(0)
    augroup END

    augroup color_overrides
        autocmd!
        " Fix the absurdly low constrast of nord-vim
        autocmd ColorScheme nord highlight Comment guifg=#7b88a1 gui=bold
        autocmd ColorScheme nord highlight Folded guifg=#7b88a1
        autocmd ColorScheme nord highlight FoldColumn guifg=#7b88a1
        autocmd ColorScheme nord highlight Conceal guifg=#7b88a1 guibg=bg
        autocmd ColorScheme nord highlight CocHighlightText guibg=#434C5E

        " Replace undercurl/line with italics
        autocmd ColorScheme * highlight CocWarningHighlight gui=italic,undercurl
            \ guifg=NONE guisp=#EBCB8B
        autocmd ColorScheme * highlight CocErrorHighlight gui=italic,undercurl
            \ guifg=NONE guisp=#BF616A
    augroup END

    if &termguicolors
        let g:nord_italic = 1
        let g:nord_underline = 1
        colorscheme nord
    endif

else
    set notermguicolors
    colorscheme desert
endif

" ========== general_settings ========== {{{1

set fileformats=unix,dos,mac
set nrformats=bin,hex,alpha
set scrolloff=4
set cmdheight=2
set wrap
set hidden
set lazyredraw
set foldmethod=indent
set foldcolumn=1
set foldopen-=block
set virtualedit=block

set number
set relativenumber

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
set noshowmode

" No annoying bells on errors
set noerrorbells
set vb t_vb=

" wildmenu settings
" complete till longest common string and list all matches,
" then complete next full match
set wildmode=list:longest,full
set wildignore=*.o,*.pyc,*.class
" enable tab completion from cnoremap
set wildcharm=<tab>

" Search/replace settings
if has('nvim')
    set inccommand=nosplit
endif
set nowrapscan
set ignorecase
set smartcase

" Space/Tab settings
" Use spaces instead of tabs
" 1 tab == 4 spaces
set expandtab
set shiftwidth=4
set softtabstop=4

" Default to system clipboard
if has('clipboard')
    set clipboard^=unnamedplus
endif

" Persistent undo
if has("persistent_undo")
    " Vim doesn't have stdpath()
    if has('nvim')
        let s:data = $HOME."/.local/share/nvim"
    else
        let s:data = $HOME."/.local/share/vim"
    endif
    let &undodir = s:data."/undo"
    let &backupdir = s:data."/backup"
    if !isdirectory(&backupdir)
        silent! call mkdir( &backupdir, "p")
    endif
    set undofile
    set backup
endif

" Commands
" Easily edit this file
command! -bar E edit $MYVIMRC

" :W sudo saves the file
" Bugged in neovim
if !has('nvim')
    command! W w !sudo tee % > /dev/null
endif

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

augroup general_autocommands
    " clear all autocmds
    autocmd!

    " Return to last edit position when opening files
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$")
      \ |   exe "normal! g`\""
      \ | endif
    " Open folds on cursor and center cursor
    autocmd BufWinEnter * normal! zv
    autocmd BufWinEnter * normal! zz

    " Autoread is bugged. Force it to update buffer
    autocmd FocusGained,BufEnter * :silent! checktime

    " Turn off relative numbers in insert mode
    autocmd InsertEnter * :set norelativenumber | :set cursorline
    autocmd InsertLeave * :set relativenumber | :set nocursorline
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
        " TODO: Make it work in visual mode too
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

" ========== mappings ========== {{{1

" Move by wrapped lines unless a count is given
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')

" tabs_windows_buffers {{{2

" TODO: Make this a command:
" echo execute("tab :split " . escape("+term top", ' '))

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
" TODO: Don't center if no match found.
" All this complicated stuff is needed so that the search count isn't erased
function! s:SearchAndCenter(forward, count) abort
    " Set `nolazyredraw` to make `:normal! n` show search count asap
    let l:lazy = &lazyredraw
    set nolazyredraw
    " Print the error ourselves so that vim doesnt output the usual
    " garbage about "Error detected while processing function"
    echohl ErrorMsg
    try
        if a:count == 0
            let l:count = 1
        else
            let l:count = a:count
        endif
        if a:forward
            execute "normal! " . l:count . "n"
        else
            execute "normal! " . l:count . "N"
        endif
    catch /^Vim\%((\a\+)\)\=:E384
        echom "E384: search hit TOP without match for: " . @/
    catch /^Vim\%((\a\+)\)\=:E385
        echom "E385: search hit BOTTOM without match for: " . @/
    endtry
    " Go back to defaults
    echohl None
    let &lazyredraw = l:lazy
    " All *that* ^ just so we can center the page and keep the search count
    normal! zz
endfunction
" Highlighting has to be set HERE because vim saves the highlighting state
" before USER FUNCTIONS and then RESTORES THEM AFTERWARDS.
noremap <silent> N :<C-u>set hlsearch \| :call <SID>SearchAndCenter(0, v:count)<cr>
noremap <silent> n :<C-u>set hlsearch \| :call <SID>SearchAndCenter(1, v:count)<cr>

" Unhighlight
nnoremap <silent> <leader><cr> :nohlsearch<cr>

" Make these go into blackhole buffer
nnoremap x "_x
nnoremap S "_S

" More consistent with d
noremap Y y$

" Remap VIM 0 to first non-blank character
noremap 0 ^
"}}}
" vim:foldmethod=marker:foldlevel=1:
