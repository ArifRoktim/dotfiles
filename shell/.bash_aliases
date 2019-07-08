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
alias l.='ls -ld'

# More detailed jobs
alias jobs='jobs -l'

alias nterm='nvim -c term'

# rust tool aliases
alias rcheck="cargo check"
alias rfmt="cargo +nightly fmt"
alias rgo="cargo run"
alias run="cargo run --release"
#==================================== FUNCTIONS ====================================

# ls after cd
function cd {
    builtin cd "$@"
    if [ $? -eq 0 ]; then
        command ls --color=auto
    fi
}

# cargo test
function rtest {
    local tests=""
    local quiet=""
    while [[ $# -gt 0 ]]; do
        if [[ "$1" = '-v' ]]; then
            quiet="-- --nocapture"
        else
            tests="${tests} $1"
        fi
        shift
    done
    local cmd="cargo test ${tests} ${quiet}"
    echo $cmd
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
