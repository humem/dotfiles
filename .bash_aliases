# -*- sh -*-

# Aliases
alias a='alias'
alias c='google-chrome >/dev/null 2>&1 &'
alias e='emacs &'
alias g='grep'
alias j='jupyter lab --ip 0.0.0.0 --no-browser'
alias l='less'

# WSL2 X Server e.g. VcXsrv
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0
