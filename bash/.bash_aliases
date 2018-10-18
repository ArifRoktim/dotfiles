#
# ~/.bash_aliases
#

# ========== ALIASES ==========

# Start neovim-remote server
alias nv='nvr -s -cc term'

alias :q='exit'

# Colored aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Some more ls aliases
alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -CF'

# More detailed jobs
alias jobs='jobs -l'
#==================================== FUNCTIONS ====================================

# ls after cd
function cd {
    if [ -z "$1" ]; then
        builtin cd
    else
        builtin cd "$1"
    fi
    if [ $? -eq 0 ]; then
        ls
    fi
}

# Start/attach to tmux session
function tnew {
    if [ -z "$1" ]; then
        tmux ls
    else
        (
        command cd;
        tmux new-session -As "$1"
        )
    fi
}

# Run in background
function ns {
    nohup "$@" &> /dev/null &
}

