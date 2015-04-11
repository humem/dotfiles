# -*- Shell-script -*-

if [ `uname -s` = 'Darwin' ]; then
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/X11R6/bin
    export MANPATH=/opt/local/share/man:$MANPATH
fi

export PS1='\h:\w$ '

if [ -r $HOME/.bashrc ]; then source $HOME/.bashrc; fi
if [ -r $HOME/.bash_local ]; then source $HOME/.bash_local; fi
