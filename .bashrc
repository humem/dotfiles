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

# Torch
alias use_torch='export PATH=/opt/torch/bin:/usr/local/cuda/bin:/usr/local/bin:/usr/bin:/bin; unset LD_LIBRARY_PATH'

# Python
export PYTHONDONTWRITEBYTECODE=1
alias i='ipython'

# ls
export LSCOLORS=gxfxcxdxbxegedabagacad
alias la='ls -alFGv'
alias ll='ls -lFGv'
alias ls='ls -FGv'

# Docker
alias neo4j='docker run -d -p 7474:7474 -v $HOME/neo4j/data:/data neo4j/neo4j'
# VBoxManage controlvm "boot2docker-vm" natpf1 "neo4j,tcp,127.0.0.1,7474,,7474"

if [ `uname -s` = 'Darwin' ]; then
    alias dev='eval $(docker-machine env dev)'
    alias e='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs -nw'
    alias v='/Applications/MacVim.app/Contents/MacOS/Vim'
    alias ql='qlmanage -p'
else
    alias e='emacs-24.4'
    alias la='ls -alF --color=auto'
    alias t='tmux a'
fi

if [ -r $HOME/.bashrc_local ]; then source $HOME/.bashrc_local; fi
