# Aliases
alias a='alias'
alias e='emacs -nw'
alias exi='exit'
alias g='grep'
alias l='less'
alias m='more'

# Locale
#export LANG=ja_JP.UTF-8
export LANG=en_US.UTF-8
unset LC_CTYPE
unset LC_ALL
#alias en='unset LANG'
alias c='export LANG=C.UTF-8'
alias en='export LANG=en_US.UTF-8'
alias ja='export LANG=ja_JP.UTF-8'

# ls (list directory contents)
export LSCOLORS=gxfxcxdxbxegedabagacad
export LS_ARGS='FGv'
alias la='ls -al$LS_ARGS'
alias ll='ls -l$LS_ARGS'
alias ls='ls -$LS_ARGS'

# Python
export PYTHONDONTWRITEBYTECODE=1
alias i='ipython --pylab'

# Keras with TensorFlow
export KERAS_BACKEND=tensorflow

# Docker
# note: combination of '--rm' and '--detach-keys' causes hung
export DOCKER_DETACH_KEYS='--detach-keys="ctrl-z,ctrl-q"'
export DOCKER_RUN_CMD='docker run $DOCKER_DETACH_KEYS -e DOCKER_CONTAINER=docker -e LANG=\$LANG -e TERM=xterm-256color -it' 
if [ $DOCKER_CONTAINER ]; then
    export PS1='$DOCKER_CONTAINER:\w$ '
fi
export NB_USER=jovyan

if [ `uname -s` = 'Darwin' ]; then
    alias e='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs -nw'
    for i in 2 1 0; do
        export IPADDR=$(ipconfig getifaddr en${i})
        if [ $IPADDR ]; then break; fi
    done
    alias ql='qlmanage -p'
    alias v='/Applications/MacVim.app/Contents/MacOS/Vim'
    # Docker
    export DOCKER_RUN_CMD="$DOCKER_RUN_CMD -e DISPLAY=\$IPADDR:0 -v $HOME:/home/$NB_USER"
    alias dev='eval $(docker-machine env dev)'
    alias xd='socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\"'
    # VBoxManage controlvm "dev" natpf1 "neo4j,tcp,127.0.0.1,7474,,7474"
else
    export IPADDR=$(hostname -I)
    alias la='ls -al$LS_ARGS --color=auto'
    alias t='tmux a'
    # Docker
    export DOCKER_RUN_CMD="$DOCKER_RUN_CMD -e DISPLAY --net=host -v \$HOME/.Xauthority:/.Xauthority -v \$HOME/.Xauthority:/root/.Xauthority -v \$HOME:\$HOME"
fi

# Docker
alias da='docker attach $DOCKER_DETACH_KEYS'
alias dr=$DOCKER_RUN_CMD
alias drm='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -a| awk "/^<none>/ { print $3 }")'
alias neo4j='docker run -d --name neo4j -p 7474:7474 -v /var/lib/neo4j/data:/data neo4j/neo4j'

if [ -r $HOME/.bashrc_local ]; then source $HOME/.bashrc_local; fi
