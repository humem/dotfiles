# -*- Shell-script -*-

if [ `uname -s` = 'Darwin' ]; then
    export PATH=/usr/local/opt/coreutils/libexec/gnubin:/usr/local/bin:/usr/local/sbin:/usr/X11/bin:$PATH
else
    if [ -f /etc/bashrc ]; then
            . /etc/bashrc
    fi
    export CUDA_PATH=/usr/local/cuda
    export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
    export PATH=$HOME/opt/bin:$CUDA_PATH/bin:$PATH
    export LD_LIBRARY_PATH=/usr/local/lib:$CUDA_PATH/lib64
    unset SSH_ASKPASS
fi

export PS1='\h:\w$ '

if [ -r $HOME/.bashrc ]; then source $HOME/.bashrc; fi
