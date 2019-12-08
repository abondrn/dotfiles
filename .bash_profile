# interaction

alias ..='cd ..'
alias ...='cd ...'
alias ....='cd ....'

alias dfn=type

# Bash history search, partial + up-arrow
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# shortcuts

alias finder='open .'
alias python2=/usr/bin/python
alias pip2=/usr/local/bin/pip

# utils

alias calc='bc -l'
roll() {
    local l=$1
    [ "$l" == "" ] && l=100
    echo $[ 1 + $[ RANDOM % ${l} ]]
}
backup() { cp $1 $1.$(date +"%Y-%m-%d_%H-%M-%S"); }
setmac() {
    sudo ifconfig eth0 down
    sudo ifconfig eth0 hw ether "$1"
    sudo ifconfig eth0 up
}

# dev env

source /usr/local/anaconda3/etc/profile.d/conda.sh

clear
