#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Set vi bindings
set -o vi

# Easy escaping
bind '"jk":"\e"'
bind '"kj":"\e"'

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

    # Number of jobs
    local JOBS=$( jobs -p | wc -l )
    PS1+="\[\e[01;36m\]$( [ $JOBS -gt 0 ] && echo [$JOBS] )"

    PS1+="\[\e[01;31m\]$( [ $EXIT -ne 0 ] && echo {$EXIT} )"    # Exit code of last run command
    PS1+="\[\e[01;32m\]\u@\h"                                   # User and hostname
    PS1+="\[\e[01;34m\]:\w\n"                                   # Current working directory
    PS1+="\D{%m/%d|%I:%M}"                                      # Date and time
    PS1+="\$\[\e[00m\] "                                        # Prompt
}

PS2=">\[\e[0m\] "

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Use bash completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

[[ -f /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion
[[ -f /etc/bash/bashrc.d/bash_completion.sh ]] && . /etc/bash/bashrc.d/bash_completion.sh

# Searches official repositories when entering an unrecognized command
. /usr/share/doc/pkgfile/command-not-found.bash

