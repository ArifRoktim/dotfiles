#
# ~/.bash_aliases
#

# ========== ALIASES ==========

# Common typos
alias :q='exit'
alias :q!='exit'
alias qgit='git'

# Colored aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Some more ls aliases
alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -F'
alias l.='ls -ld'

alias nterm='nvim -c term'

# rust tool aliases
alias rcheck='cargo check'
alias rtest='cargo test'
alias rdoc='cargo doc --no-deps'
alias rfmt='cargo fmt'
alias rgo='cargo run'
alias run='cargo run --release'
#==================================== FUNCTIONS ====================================

# ls after cd
function cd {
    builtin cd "$@"
    if [ $? -eq 0 ]; then
        command ls --color=auto
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
