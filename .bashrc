# Aliases
alias a='alias'
alias exi='exit'
alias g='grep'
alias l='less'
alias m='more'

# Locale
#export LANG=ja_JP.UTF-8
export LANG=C.UTF-8
unset LC_CTYPE
#alias en='unset LANG'
alias en='export LANG=C.UTF-8'
alias ja='export LANG=ja_JP.UTF-8'

# ls
export LSCOLORS=gxfxcxdxbxegedabagacad
alias la='ls -alFGv'
alias ll='ls -lFGv'
alias ls='ls -FGv'

# Python
export PYTHONDONTWRITEBYTECODE=1
alias i='ipython'

# Torch
alias use_torch='export PATH=/opt/torch/bin:/usr/local/cuda/bin:/usr/local/bin:/usr/bin:/bin; unset LD_LIBRARY_PATH'

# Docker
export DOCKER_RUN_CMD='docker run -e LANG=$LANG -it --rm -v $HOME:$HOME'

if [ `uname -s` = 'Darwin' ]; then
    alias e='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs -nw'
    for i in 2 1 0; do
        export IPADDR=$(ipconfig getifaddr en${i})
        if [ $IPADDR ]; then break; fi
    done
    alias ql='qlmanage -p'
    alias v='/Applications/MacVim.app/Contents/MacOS/Vim'
    # Docker
    export DOCKER_RUN_CMD="$DOCKER_RUN_CMD -e DISPLAY=\$IPADDR:0"
    alias dev='eval $(docker-machine env dev)'
    alias xd='socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\"'
else
    if [ $(which emacs-24.4) ]; then
        alias e='emacs-24.4 -nw'
    else
        alias e='emacs -nw'
    fi
    export IPADDR=$(hostname -I)
    alias la='ls -alF --color=auto'
    alias t='tmux a'
    # Docker
    export DOCKER_RUN_CMD="$DOCKER_RUN_CMD -e DISPLAY --net=host -v \$HOME/.Xauthority:/.Xauthority:rw -v \$HOME/.Xauthority:/root/.Xauthority:rw"
fi

# Docker
alias dr=$DOCKER_RUN_CMD
alias drm='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -a| awk "/^<none>/ { print $3 }")'
alias neo4j='docker run -d --name neo4j -p 7474:7474 -v /var/lib/neo4j/data:/data neo4j/neo4j'
# VBoxManage controlvm "boot2docker-vm" natpf1 "neo4j,tcp,127.0.0.1,7474,,7474"

if [ -r $HOME/.bashrc_local ]; then source $HOME/.bashrc_local; fi
