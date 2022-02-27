# -*- sh -*-

if [ $WSL_DISTRO_NAME ]; then
    # X Server e.g. VcXsrv
    # WSL1
    #export DISPLAY="localhost:0.0"
    # WSL2
    export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0
fi

# fix for emacs dbind-WARINING
export NO_AT_BRIDGE=1

# Language
export LANG=C.UTF-8
export LANGUAGE=C.UTF-8
export LC_ALL=C.UTF-8

# Prompt: use brighter color for current directory
export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$ "
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

# Python
export PYTHONDONTWRITEBYTECODE=1

# Terminal
export TERM=xterm-24bit

# Aliases
alias a='alias'
alias ch='google-chrome >/dev/null 2>&1 &'
alias e='emacs &'
alias en='emacs -nw'
alias g='grep'
alias jl='jupyter lab --ip 0.0.0.0 --no-browser'
alias l='less'
alias m='more'
alias nv='nvidia-smi -l'
alias t='tmux a'
alias tb='tensorboard --logdir'

# pyenv for Visual Studio Code (VSCode)
#if [ ! "`echo $PATH | grep '.pyenv'`" ]; then
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
#fi
#eval "$(pyenv init -)"

# CUDA Toolkit
export CUDA_VERSION=cuda-11.1
export PATH="/usr/local/$CUDA_VERSION/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/$CUDA_VERSION/lib64:$LD_LIBRARY_PATH"
# or, add /usr/local/cuda-11.1/lib64 to /etc/ld.so.conf and run ldconfig as root
# To uninstall the CUDA Toolkit, run cuda-uninstaller in /usr/local/cuda-11.1/bin
