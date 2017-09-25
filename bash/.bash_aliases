#
# ~/.bash_aliases
#

# ========== ALIASES ==========

# Use neovim
alias vim='nvim'
# Start neovim-remote server
alias nv='nvr -s -cc term'

# Enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

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

alias bart='ssh -t clyde "ssh bart"'

#==================================== FUNCTIONS ====================================

# Start nvim server in a detachable session
function ac {
    acID=$RANDOM
    export NVIM_LISTEN_ADDRESS="/tmp/nvimsocket$acID"
    abduco -c $acID nvim -c term
}

function update {
    news 3
    local input;
    read -p "$( echo -e "\e[1;37mProceed? [Y/n]\e[0m " )" input
    [ $input != "Y" ] && [ $input != "y" ] && return 1
    pacaur -Sc --noconfirm
    pacaur -Syyu --needed --devel
}

# print arch news
function news {
    local output="$(curl -s "https://www.archlinux.org/feeds/news/" | xmlstarlet sel -T -t -m /rss/channel/item -v "concat(pubDate,': ',title)" -n | head -n $1)"
    echo -e "\e[1;31mhttps://www.archlinux.org/"
    echo "$output"
    echo -en "\e[0m"
}

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

# Run in background
function ns {
    nohup "$@" 2>&1 > /dev/null &
}

