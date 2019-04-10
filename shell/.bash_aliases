#
# ~/.bash_aliases
#

# ========== ALIASES ==========

alias :q='exit'
alias :q!='exit'

# Colored aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Some more ls aliases
alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -F'

# More detailed jobs
alias jobs='jobs -l'
#==================================== FUNCTIONS ====================================

# ls after cd
function cd {
    builtin cd "$@"
    if [ $? -eq 0 ]; then
        command ls --color=auto
    fi
}

# Start/attach to tmux session
function tnew {
    if [ -z "$1" ]; then
        tmux ls
    else
        (
        command cd;
        COLORTERM=truecolor tmux new-session -As "$1"
        )
    fi
}

# Run in background
function ns {
    nohup "$@" &> /dev/null &
}

function :qa {
    if [[ -n $NVIM_LISTEN_ADDRESS ]] && type nvr &> /dev/null; then
        nvr -c qa
    else
        exit
    fi
}

function :qa! {
    if [[ -n $NVIM_LISTEN_ADDRESS ]] && type nvr &> /dev/null; then
        nvr -c qa!
    else
        exit
    fi
}
