# -*- sh -*-

# X Server e.g. VcXsrv
# WSL1
#export DISPLAY="localhost:0.0"
# WSL2
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0

# fix for emacs dbind-WARINING
export NO_AT_BRIDGE=1

# Prompt: use brighter color for current directory
export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$ "
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

# Python
export PYTHONDONTWRITEBYTECODE=1

# Aliases
alias a='alias'
alias c='google-chrome >/dev/null 2>&1 &'
alias e='emacs &'
alias g='grep'
alias j='jupyter lab --ip 0.0.0.0 --no-browser'
alias l='less'
alias m='more'
alias t='tmux a'
