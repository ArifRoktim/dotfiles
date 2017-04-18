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

    # Number of jobs
    local JOBS=$( jobs -p | wc -l )
    PS1+="\[\e[01;36m\]$( [ $JOBS -gt 0 ] && echo [$JOBS] )"

    PS1+="\[\e[01;31m\]$( [ $EXIT -ne 0 ] && echo {$EXIT} )"    # Exit code of last run command
    PS1+="\[\e[01;32m\]\u@\h"                                   # User and hostname
    PS1+="\[\e[01;34m\]:\w"                                     # Current working directory
    local branch="$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/')"
    PS1+="\[\e[01;35m\]$branch"                                 # Show current git branch
    PS1+="\n"
    PS1+="\D{%m/%d|%I:%M}"                                      # Date and time
    PS1+="\$\[\e[00m\] "                                        # Prompt
}



PS2=">\[\e[0m\] "

# Load aliases
[[ -f ~/.bash_aliases ]] && . ~/.bash_aliases

# Use bash completion
[[ -f /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion || true

# Searches official repositories when entering an unrecognized command
[[ -f /usr/share/doc/pkgfile/command-not-found.bash ]] && . /usr/share/doc/pkgfile/command-not-found.bash || true

