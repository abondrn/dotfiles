# https://opensource.com/article/19/7/bash-aliases

if [ -e ../subrepos/bash-sensible/sensible.sh ]; then
    source ../subrepos/bash-sensible/sensible.sh
fi


# Bash history search, partial + up-arrow
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'


# Traversing up nested directories
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Change directories and view the contents at the same time, avoids start-and-stop
function cl() {
    DIR="$*";
        # if no DIR given, go home
        if [ $# -lt 1 ]; then
                DIR=$HOME;
    fi;
    builtin cd "${DIR}" && \
    # use your preferred ls command
        ls -F --color=auto
}


# Add safety nets
# do not delete / or prompt if deleting more than 3 files at a time #
alias rm='rm -I --preserve-root'
 
# confirmation #
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

alias toss='mv --force -t ~/.Trash '
alias empty="sudo rm -rf ~/.Trash/*"                        # Empty macOS trash


# reboot / halt / poweroff
alias afk='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'
alias poweroff="sudo shutdown -h now"
alias restart="sudo reboot"


# Colorize output
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'


# shortcuts

alias gs='git status'
alias gcm='git commit -m'
alias gcb='git checkout -b'
alias finder='open .'
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


function topcmd() {
    history | \
        awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | \
        grep -v "./" | \
        column -c3 -s " " -t | \
        sort -nr | \
        nl | \
        head -n10
}

# show shorted files
alias big='ls --human-readable --size -1 -S --classify'
alias recent='ls -t -1'

alias filecount='find . -type f | wc -l'

alias forgot='history|grep'

# cp with progress bar, like pv
alias cpv='rsync -ah --info=progress2'


alias dclear='docker system prune -a'

function dssh(){
   docker exec -it $1 /bin/bash
}


export EDITOR='code'