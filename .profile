# -*- Shell-script -*-

if [ `uname -s` = 'Darwin' ]; then
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/X11R6/bin
    export MANPATH=/opt/local/share/man:$MANPATH
else
    if [ -f /etc/bashrc ]; then
            . /etc/bashrc
    fi
    export CUDA_DIR=/usr/local/cuda
    export PATH=/opt/anaconda/bin:/usr/local/cuda/bin:$PATH
    export LD_LIBRARY_PATH=/opt/anaconda/lib:/usr/local/lib:/usr/local/cuda/lib64:$LD_LIBRARY_PATH
    unset SSH_ASKPASS
fi

export PS1='\h:\w$ '

if [ -r $HOME/.bashrc ]; then source $HOME/.bashrc; fi
