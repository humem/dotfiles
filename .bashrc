# Aliases
alias a='alias'
alias exi='exit'
alias g='grep'
alias l='less'
alias m='more'

# Locale
export LANG=ja_JP.UTF-8
unset LC_CTYPE
alias en='unset LANG'
alias ja='export LANG=ja_JP.UTF-8'

# Python
export PYTHONDONTWRITEBYTECODE=1
alias i='ipython'

# ls
export LSCOLORS=gxfxcxdxbxegedabagacad
alias la='ls -alFGv'
alias ll='ls -lFGv'
alias ls='ls -FGv'

# Docker
alias dev='eval $(docker-machine env dev)'

if [ `uname -s` = 'Darwin' ]; then
    alias e='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs -nw'
    alias v='/Applications/MacVim.app/Contents/MacOS/Vim'
    alias ql='qlmanage -p'
else
    alias e='emacs-24.4'
    alias la='ls -alF --color=auto'
    alias t='tmux a'
fi

if [ -r $HOME/.bashrc_local ]; then source $HOME/.bashrc_local; fi