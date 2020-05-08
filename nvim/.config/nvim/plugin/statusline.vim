"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"   => Attribution
"   => Nord_vim
"   => modes_dictionary
"   => statusline_functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Attribution {{{1
" Inspired by: bluz71/vim-moonfly-statusline
" URL:          github.com/bluz71/vim-moonfly-statusline
" License:      MIT (https://opensource.org/licenses/MIT)
" }}}1

if exists("g:loaded_statusline")
    finish
endif
let g:loaded_statusline = 1

" Nord_vim colors (https://github.com/arcticicestudio/nord-vim) {{{1
let s:nord0_gui = "#2E3440"
let s:nord1_gui = "#3B4252"
let s:nord2_gui = "#434C5E"
let s:nord3_gui = "#4C566A"
let s:nord4_gui = "#D8DEE9"
let s:nord5_gui = "#E5E9F0"
let s:nord6_gui = "#ECEFF4"
let s:nord7_gui = "#8FBCBB"
let s:nord8_gui = "#88C0D0"
let s:nord9_gui = "#81A1C1"
let s:nord10_gui = "#5E81AC"
let s:nord11_gui = "#BF616A"
let s:nord12_gui = "#D08770"
let s:nord13_gui = "#EBCB8B"
let s:nord14_gui = "#A3BE8C"
let s:nord15_gui = "#B48EAD"

let s:nord1_term = "0"
let s:nord3_term = "8"
let s:nord5_term = "7"
let s:nord6_term = "15"
let s:nord7_term = "14"
let s:nord8_term = "6"
let s:nord9_term = "4"
let s:nord10_term = "12"
let s:nord11_term = "1"
let s:nord12_term = "11"
let s:nord13_term = "3"
let s:nord14_term = "2"
let s:nord15_term = "5"

" Extrapolated missing colors from nord.vim
"let s:nord0_term = ""
let s:nord2_term = "8"
let s:nord4_term = "NONE"

" modes_dictionary {{{1
let s:modes={
            \ 'n'  : ["%1*", ' Normal '],
            \ 'c'  : ["%1*", ' Command '],
            \ 'r'  : ["%1*", ' Command '],
            \ 'i'  : ["%2*", ' Insert '],
            \ 'R'  : ["%2*", ' Replace '],
            \ 't'  : ["%3*", ' Terminal '],
            \ 'v'  : ["%4*", ' Visual '],
            \ 'V'  : ["%4*", ' Visual '],
            \ '' : ["%4*", ' Visual '],
            \ 's'  : ["%4*", ' Visual '],
            \ 'S'  : ["%4*", ' Visual '],
            \ '' : ["%4*", ' Visual '],
            \ }

" statusline_functions {{{1
function! ModeText(mode) abort
    let l:ret = get(s:modes, a:mode, ["%*1"])[0]
    let l:ret .= get(s:modes, a:mode, ["", " Normal "])[1]
    return l:ret
endfunction

function! GetGit() abort
    " Turns [Git(master)]           into    (master
    " and   [Git:0123456(master)]   into    0123456(master
    " Don't show git info if fugitive isn't loaded, if we're not in a git repo
    " or if we're not editing a file
    if !exists("g:loaded_fugitive") || !exists("b:git_dir") ||
                \ expand('%') == "" || &buftype == "nofile"
        return ""
    endif
    let l:ret = FugitiveStatusline()
    if l:ret == ""
        return ""
    else
        return substitute(l:ret,
                    \ '\[Git\%[:]\(.*\)(\(.\{-\}\))\]', '\1(\2 ', "")
    endif
endfunction

function! GetCwd() abort
    " Displays cwd of current tab iff we're viewing a file on the fs that's
    " been specified with a relative path
    let l:file = expand('%')

    " !~# '^\w\+://'    => File is on filesystem. Ex: Won't match fugitive://...
    " !~# '^/'          => File name isn't an absolute path
    " != "" && != "nofile"   => Buffer has a file
    if l:file !~# '^\w\+://' && l:file !~# '^/' &&
                \ l:file != "" && &buftype != "nofile"
        return pathshorten(fnamemodify(getcwd(0, 0), ":~"))
    endif
    return ''
endfunction

" Returns pathshorten()'d filename and removes sha id from fugitive files
function! GetFile() abort
    let l:file = substitute(expand('%'), $HOME, '~', '')
    "let l:bufnr = bufnr('%') . ':'

    if exists("g:loaded_fugitive") && l:file =~# '^fugitive://'
        " File is a fugitive object so return just the file path
        " Remove the sha id
        let l:nohash = substitute(l:file,
                    \ '^fugitive://\(.\{-\}\).git//\w\{40}', 'fugitive://\1', "")
        let l:short = "f:" . pathshorten(split(l:nohash, 'fugitive:')[0])
        return l:short
    elseif l:file == ""
        return '[No Name]'
    else
        return pathshorten(l:file)
    endif
endfunction

function! GetBufnr() abort
    let l:bufnr = bufnr('%') . ":"
    return l:bufnr
endfunction

function! s:UserColors() abort
    " Highlight groups:
    " User1 -> Normal, Command
    " User2 -> Insert, Replace
    " User3 -> Terminal
    " User4 -> Visual (and select LOL)
    exec "highlight User1 guifg=" . s:nord4_gui  . " ctermfg=" . s:nord4_term
    exec "highlight User2 guifg=" . s:nord8_gui  . " ctermfg=" . s:nord8_term
    exec "highlight User3 guifg=" . s:nord14_gui . " ctermfg=" . s:nord14_term
    exec "highlight User4 guifg=" . s:nord15_gui . " ctermfg=" . s:nord15_term
endfunction

function! ActiveStatusLine() abort
    return s:MyStatusLine("current")
endfunction

function NotCurrentStatusLine() abort
    return s:MyStatusLine("not-current")
endfunction

function! s:MyStatusLine(mode) abort
    let l:statusline = ''

    if a:mode == "current"
        let l:statusline .= ModeText(mode())
        let l:statusline .= "%#StatusLineNC#"
        let l:statusline .= "%{len(GetCwd())?' '.GetCwd():''}"
    else
        let l:statusline .= "%1*"
        " FIXME: Bug? 1st space will be ignored in not-current windows.
        " 2 spaces are treated as 1, 3 spaces as 2, etc.
        let l:statusline .= "%{len(GetCwd())?'  '.GetCwd():''}"
    endif

    let l:statusline .= " %{GetGit()}"
    let l:statusline .= "%*"
    let l:statusline .= " %{bufnr('%')}"
    let l:statusline .= "%<:"
    let l:statusline .= "%{GetFile()}"
    let l:statusline .= "%{&readonly?'!':''}"
    let l:statusline .= "%{&modified?'*':''}"
    let l:statusline .= " %="
    let l:statusline .= "[%02.l,%02.c]"
    let l:statusline .= "[%02.p%%]"

    return l:statusline
endfunction

function! s:StatusLine(mode) abort
    if a:mode == "command"
        " Use default statusline for cmd line window
        return
    elseif a:mode == "current"
        setlocal statusline=%!ActiveStatusLine()
    else
        setlocal statusline=%!NotCurrentStatusLine()
    endif
endfunction

augroup MyStatusline
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter   * call s:StatusLine("current")
    autocmd WinLeave                        * call s:StatusLine("not-current")
    autocmd CmdwinEnter,CmdlineEnter        * call s:StatusLine("command") | redraw
    autocmd ColorScheme,SourcePre           * call s:UserColors()
augroup END

" }}}1

call s:UserColors()
" vim:foldmethod=marker
