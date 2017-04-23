#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source host specific config file
[[ -f ~/.bash_local ]] && . ~/.bash_local || true

# Set vi bindings
set -o vi

# Easy escaping
bind '"jk":"\e\e"'
bind '"kj":"\e\e"'

# C-k to view jobs
bind -x '"\C-k":"jobs"'

# Use nvim for sudoedit
export EDITOR=nvim
export VISUAL=nvim

# Add some directories to PATH
export PATH=$HOME/.local/bin:$PATH:/usr/bin/core_perl

shopt -s checkwinsize                       # Check the window size after each command and update the values of LINES and COLUMNS.
shopt -s histappend                         # append to the history file
shopt -s histverify                         # Allow editing of history expansion
export HISTCONTROL=ignoreboth:erasedups     # Don't add commands that are duplicated or start with a space

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

    PS1+="${red}$( [ $EXIT -ne 0 ] && echo {$EXIT} )"    # Exit code of last run command
    PS1+="${green}\u@\h"                                   # User and hostname
    PS1+="${blue}:\w"                                     # Current working directory
    local branch="$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/')"
    PS1+="${magenta}${branch}"                                 # Show current git branch
    PS1+="\n"
    PS1+="\D{%m/%d|%I:%M}"                                      # Date and time
    PS1+="\$${reset} "                                        # Prompt
}



PS2=">\[\e[0m\] "

# Load aliases
[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases

# Use bash completion
[[ -f /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion || true

# Searches official repositories when entering an unrecognized command
[[ -f /usr/share/doc/pkgfile/command-not-found.bash ]] && . /usr/share/doc/pkgfile/command-not-found.bash || true

