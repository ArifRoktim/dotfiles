#
# ~/.bashrc
#

# Append paths
insertpath () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="$1:${PATH:+$PATH}"
    esac
}

insertpath "$HOME/.local/bin"
insertpath "$HOME/.cargo/bin"
unset insertpath
export PATH

# Use nvim for editor
if type nvim &> /dev/null; then
    export EDITOR="nvim"
elif type vim &> /dev/null; then
    export EDITOR="vim"
elif type vi &> /dev/null; then
    export EDITOR="vi"
elif type nano &> /dev/null; then
    export EDITOR="nano"
fi

# Source host specific config file
if [[ -f ~/.bash_local ]]; then
    source ~/.bash_local
fi

# If not running interactively, skip rest of file
[[ $- != *i* ]] && return

[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases

# git info in prompt
[[ -f ~/dotfiles/git-prompt.sh ]] && . ~/dotfiles/git-prompt.sh

# set python repl prompt
[[ -f ~/.config/pyrc ]] && export PYTHONSTARTUP="${HOME}/.config/pyrc"

if [[ -f /usr/share/doc/pkgfile/command-not-found.bash ]]; then
    . /usr/share/doc/pkgfile/command-not-found.bash
fi

# Some man page coloring
export LESS_TERMCAP_md=$'\e[1;34m'

# Set vi bindings
set -o vi

# Dir colors
if [[ -f "$HOME/dotfiles/dir_colors" ]]; then
    eval $(dircolors -b "$HOME/dotfiles/dir_colors")
fi

# Disable Ctrl-s and Ctrl-Q control flow
stty -ixon
shopt -s checkwinsize                       # Check the window size after each command and update the values of LINES and COLUMNS.
shopt -s histappend                         # append to the history file
shopt -s histverify                         # Allow editing of history expansion
shopt -s dotglob                            # Include hidden files for globs
export HISTCONTROL=ignoreboth:erasedups     # Don't add commands that are duplicated or start with a space
export HISTTIMEFORMAT='%F %T '

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

#PS1='$([ \j -gt 0 ] && echo [\j])\u@\h:\w\$ '       # Uncolored prompt
PROMPT_COMMAND=_prompt_command                      # Colored prompt

_prompt_command() {

    local EXIT="$?"                                             # Save exit code of last run command; this must be first
    PS1=''                                                      # Reset prompt

    # ANSI escape sequences
    # Attributes
    local reset="\[\e[0m\]"
    local bold="\[\e[1m\]"
    # FG Colors
    local black="\[\e[30m\]"
    local red="\[\e[31m\]"
    local green="\[\e[32m\]"
    local yellow="\[\e[33m\]"
    local blue="\[\e[34m\]"
    local magenta="\[\e[35m\]"
    local cyan="\[\e[36m\]"
    local white="\[\e[37m\]"
    # BG Colors
    local bgBlack="\[\e[40m\]"
    local bgRed="\[\e[41m\]"
    local bgGreen="\[\e[42m\]"
    local bgYellow="\[\e[43m\]"
    local bgBlue="\[\e[44m\]"
    local bgMagenta="\[\e[45m\]"
    local bgCyan="\[\e[46m\]"
    local bgWhite="\[\e[47m\]"

    # Number of jobs
    local JOBS=$( jobs -p | wc -l )
    PS1+="${bold}${cyan}$( [ $JOBS -gt 0 ] && echo [$JOBS] )"

    PS1+="${red}$( [ $EXIT -ne 0 ] && echo {$EXIT} )"       # Exit code of last run command
    # User and hostname
    PS1+="${green}\u"
    PS1+="$hostcolor@"                                      # $hostcolor defined in ~/.bash_local
    PS1+="\h${reset}"

    PS1+="${bold}${blue}: \w"                               # Current working directory





    local git_info="$(GIT_PS1_SHOWDIRTYSTATE='1' GIT_PS1_SHOWSTASHSTATE='1' \
        GIT_PS1_SHOWUPSTREAM='auto' __git_ps1 ' (%s)')"
    PS1+="${magenta}${git_info}"


    PS1+="\n"
    PS1+="\D{%m/%d|%I:%M}"                                  # Date and time
    PS1+="\$${reset} "                                      # Prompt

    # Set prompt 2
    PS2=">${reset} "
}

# Change tty colors
if [ "$TERM" = "linux" ]; then
    # black
    echo -en "\e]P0073642" # make background blue
    # red
    echo -en "\e]P9FF0000"
    # green
    # yellow
    echo -en "\e]P3FFFF00"
    # blue
    echo -en "\e]P43465a4"
    echo -en "\e]PC4d79ff"
    # magenta
    # cyan
    # white
    echo -en "\e]P7FFFFFF"
    clear
fi

