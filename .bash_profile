# interaction

# Bash history search, partial + up-arrow
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# shortcuts

alias refresh='source ~/.bash_profile'

alias ..='cd ..'
alias ...='cd ...'
alias ....='cd ....'

alias dfn=type
alias enval='printenv | grep'
alias loc="find . -name '*.py' | xargs wc -l | sort -n"

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

alias python=/usr/local/bin/python3
alias python2=/usr/bin/python
alias pip2=/usr/local/bin/pip
alias flake8stats="flake8 --statistics | sort -n"

clean() {
    brew cleanup
    rm -rf $(brew --cache) /Library/Cache ~/Library/Cache $TMPDIR
}

# ==> openblas
# openblas is keg-only, which means it was not symlinked into /usr/local,
# because macOS provides BLAS in Accelerate.framework.

# For compilers to find openblas you may need to set:
export LDFLAGS="-L/usr/local/opt/openblas/lib"
export CPPFLAGS="-I/usr/local/opt/openblas/include"

# For pkg-config to find openblas you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/openblas/lib/pkgconfig"

# for llvmlites
export PATH="/usr/local/opt/llvm@9/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/llvm@9/lib"
export CPPFLAGS="-I/usr/local/opt/llvm@9/include"
export LLVM_CONFIG="/usr/local/opt/llvm@9/bin/llvm-config"