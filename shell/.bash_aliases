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
alias la='ls -AF'
alias l='ls -F'
alias l.='ls -ld'

alias nterm='nvim -c term'
alias waste='du -sh * | sort -h'

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

# Run `tree -a` while using .gitignore
function gtree {
    # locate .gitignore file
    ignore="$(git rev-parse --show-toplevel 2> /dev/null)/.gitignore"

    if [[ $? -ne 0 ]]; then
        echo "Not a git repo" 1>&2
        return 1
    fi

    pattern=".git"
    while read line; do
        pattern="${pattern}|${line}"
    done < "$ignore"

    tree -aI "$pattern"
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
