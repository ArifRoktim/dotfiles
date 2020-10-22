#
# ~/.bash_aliases
#

# ========== ALIASES ==========

# Common typos
alias :q='exit'
alias :q!='exit'
alias :qa='exit'
alias :qa!='exit'
alias qgit='git'

# Colored aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Some more ls aliases
alias ll='ls -alhF'
alias la='ls -AF'
alias l='ls -F'
alias l.='ls -ld'

alias nterm='history -a; nvim +term'
alias fugitive='nvim +Git +only'
alias view='nvim +view'
# Modify behavior of some commands when inside vim's terminal
if [[ -n $NVIM_LISTEN_ADDRESS ]] && type nvr &> /dev/null; then
    # Don't run vim within vim
    alias nvim='nvr --remote-wait-silent'
    # Quit vim from within its terminal emulator
    alias :qa='nvr -c qa'
    alias :qa!='nvr -c qa!'
    # Open new split for man page
    function sman {
        nvr -c Man\ "$@"
    }
    function vman {
        nvr -c vertical\ Man\ "$@"
    }
fi

# Misc
alias waste='du -sh * | sort -h'
alias hdmesg='dmesg --human --ctime'
alias cpr='rsync --archive -hh --partial --info=progress2'
alias listpaths='for dir in ${PATH//:/ }; do echo "$dir"; done'
alias hist-update='history -a'
alias hist-get='history -n'
# Be safer
alias rm='rm --one-file-system -I'

#==================================== FUNCTIONS ====================================

# ls after cd
function cd {
    builtin cd "$@"
    if [ $? -eq 0 ]; then
        command ls --color=auto
    fi
}

# cd to real directory that contains the symbolic link
function contains {
    if [[ $# -lt 1 ]]; then
        realdir="$(realpath -e .)"
    else
        realfile="$(realpath -e $1)"
        realdir="${realfile%/*}"
    fi
    cd -Pe "$realdir"
}

# cd to top level of repo
function gcd {
    directory="$(git rev-parse --show-toplevel 2> /dev/null)"

    if [[ $? -ne 0 ]]; then
        echo "Not a git repo" 1>&2
        return 1
    else
        cd "$directory"
    fi
}

# Run `tree -a` while using .gitignore
function gtree {
    # locate .gitignore file
    ignore="$(git rev-parse --show-toplevel 2> /dev/null)/.gitignore"

    # Check for existence of .gitignore file
    if [[ ! -e "$ignore" ]]; then
        tree -aI ".git"
        return
    fi

    pattern=".git"
    while read line; do
        # Remove leading and trailing `/`
        line="${line#/}"
        line="${line%/}"
        pattern="${pattern}|${line}"
    done < "$ignore"

    tree -aI "$pattern"
}
