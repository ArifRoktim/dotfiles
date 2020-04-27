"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"   => Nord_vim
"   => modes_dictionary
"   => statusline_functions
"       => ModeColor
"       => ModeText
"       => StripGit
"       => ReplaceHome
"       => GetCwd
"       => GetFile
"       => UserColors
"       => ActiveStatusline
"       => s:StatusLine
"       => augroup
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
            \ 'n'  : ["%2*", ' Normal '],
            \ 'c'  : ["%2*", ' Command '],
            \ 'r'  : ["%2*", ' Command '],
            \ 'i'  : ["%3*", ' Insert '],
            \ 'R'  : ["%3*", ' Replace '],
            \ 't'  : ["%4*", ' Terminal '],
            \ 'v'  : ["%5*", ' Visual '],
            \ 'V'  : ["%5*", ' Visual '],
            \ '' : ["%5*", ' Visual '],
            \ 's'  : ["%5*", ' Visual '],
            \ 'S'  : ["%5*", ' Visual '],
            \ '' : ["%5*", ' Visual '],
            \ }

" statusline_functions {{{1
function! ModeColor(mode) abort "{{{2
    return get(s:modes, a:mode, ["%*1"])[0]
endfunction

function! ModeText(mode) abort "{{{2
    return get(s:modes, a:mode, ["", " Normal "])[1]
endfunction

function! StripGit() abort "{{{2
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

" TODO: Remove this. Replace with fnamemodify()
function! ReplaceHome(input) abort "{{{2
    return substitute(a:input, $HOME, '~', '')
endfunction

function! GetCwd() abort "{{{2
    " Displays cwd of current tab iff we're viewing a file on the fs that's
    " been specified with a relative path
    " All names have $HOME replaced by ~
    let l:file = expand('%')

    " !~# '^\w\+://'    => File is on filesystem. Ex: Won't match fugitive://...
    " !~# '^/'          => File name isn't an absolute path
    " != "" && != "nofile"   => Buffer has a file
    if l:file !~# '^\w\+://' && l:file !~# '^/' &&
                \ l:file != "" && &buftype != "nofile"
        return pathshorten(ReplaceHome(getcwd(-1, 0)))
    endif
    return ''
endfunction

function! GetFile() abort "{{{2
    " TODO: Make buf# always vis
    " Returns file name modified for easier viewing along with buffer #
    " All names have $HOME replaced by ~
    " Changes fugitive file paths by removing the 40 character sha id
    " All file names are pathshorten()'d
    let l:file = expand('%')
    let l:shortfile = ReplaceHome(l:file)
    let l:bufnr = bufnr('%') . ':'

    if exists("g:loaded_fugitive") && l:file =~# '^fugitive://'
        " File is a fugitive object so return just the file path
        " Remove the sha because it's too long.
        let l:nohash = substitute(l:shortfile,
                    \ '^fugitive://\(.\{-\}\).git//\w\{40}', 'fugitive://\1', "")
        let l:short = "f:" . pathshorten(split(l:nohash, 'fugitive:')[0])
        return l:bufnr . l:short
    elseif l:file == ""
        return '[No Name]'
    else
        return l:bufnr . pathshorten(l:shortfile)
    endif
endfunction

function! s:UserColors() abort "{{{2
    " Highlight groups:
    " User1 -> I don't know why I skipped this but I don't want to change it
    " User2 -> Normal, Command
    " User3 -> Insert, Replace
    " User4 -> Terminal
    " User5 -> Visual (and select LOL)
    exec "highlight User2 guifg=" . s:nord4_gui  . " ctermfg=" . s:nord4_term
    exec "highlight User3 guifg=" . s:nord8_gui  . " ctermfg=" . s:nord8_term
    exec "highlight User4 guifg=" . s:nord14_gui . " ctermfg=" . s:nord14_term
    exec "highlight User5 guifg=" . s:nord15_gui . " ctermfg=" . s:nord15_term
endfunction

function! ActiveStatusLine() abort "{{{2
    " Statusline for active window
    let l:statusline = ''
    let l:mode = mode()

    let l:statusline .= ModeColor(l:mode)
    let l:statusline .= ModeText(l:mode)
    let l:statusline .= '%<'
    let l:statusline .= '%#StatusLineNC#'
    let l:statusline .= '%{len(GetCwd())?" ".GetCwd():""}'
    let l:statusline .= ' %{StripGit()}'
    let l:statusline .= '%*'
    let l:statusline .= ' %{GetFile()}'
    let l:statusline .= '%{&readonly?"!":""}%{&modified?"*":""} '
    let l:statusline .= '%=[%02.l,%02.c][%02.p%%]'

    return l:statusline
endfunction

function! s:StatusLine(mode) abort "{{{2
    if a:mode == "not-current"
        setlocal statusline=
        setl statusline+=%2*
        setl statusline+=\ %{len(GetCwd())?GetCwd().'\ ':''}
        setl statusline+=%{StripGit()}
        setl statusline+=%*
        setl statusline+=\ %{GetFile()}
        setl statusline+=%{&readonly?'!':''}
        setl statusline+=%{&modified?'*':''}
        setl statusline+=\ %=                   " seperation point
        setl statusline+=[%02.l,%02.c]          " line and column number
        setl statusline+=[%02.p%%]              " percent through file
    elseif a:mode == "command"
        " Use default statusline for cmd line window
        return
    else
        setlocal statusline=%!ActiveStatusLine()
    endif
endfunction

augroup MyStatusline "{{{2
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter   * call s:StatusLine("normal")
    autocmd WinLeave,FilterWritePost        * call s:StatusLine("not-current")
    autocmd CmdwinEnter,CmdlineEnter        * call s:StatusLine("command") | redraw
    autocmd SourcePre                       * call s:UserColors()
augroup END

" }}}1

call s:UserColors()
" vim:foldmethod=marker
