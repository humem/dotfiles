# -*- sh -*-

# Aliases
alias a='alias'
alias ak='autokey 2>/dev/null &'
alias ch='google-chrome >/dev/null 2>&1 &'
alias d='docker'
alias docker_start='sudo service docker start'
alias e='emacs &'
alias en='emacs -nw'
alias fc='fcitx 2>/dev/null'
alias g='git'
alias jl='jupyter lab --ip 0.0.0.0 --no-browser'
alias l='less'
alias ll='ls -alF --color'
alias m='more'
alias nv='nvidia-smi -l'
alias r='grep'
alias t='tmux a'
alias ta='gnome-terminal; fcitx 2>/dev/null; autokey 2>/dev/null &'
alias tb='tensorboard --bind_all --logdir'
alias term='gnome-terminal'
alias upd='sudo apt update'
alias upg='sudo apt upgrade'
alias v='source .venv/bin/activate'

# CUDA Toolkit
CUDA_VERSION=11.5
if [ -d "/usr/local/cuda-$CUDA_VERSION" ]; then
    export PATH="/usr/local/cuda-$CUDA_VERSION/bin:$PATH"
    export LD_LIBRARY_PATH="/usr/local/cuda-$CUDA_VERSION/lib64:$LD_LIBRARY_PATH"
fi

# Input Method
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export DefaultIMModule=fcitx

# Language
export LANG=C.UTF-8
export LANGUAGE=C.UTF-8
export LC_ALL=C.UTF-8

# Prompt: use brighter color for current directory
#export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$ "
export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;36m\]\w\[\033[00m\]\$ "

# pyenv
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# Python venv
if [ -d "$HOME/.venv" ]; then
    source $HOME/.venv/bin/activate
fi

# Python
export PYTHONDONTWRITEBYTECODE=1

# Rust
if [ -d "$HOME/.cargo" ]; then
    source $HOME/.cargo/env
fi

# Terminal
export TERM=tmux-256color
# export TERM=xterm-256color
# export TERM=xterm-24bit

# WSL
if [ $WSL_DISTRO_NAME ]; then
    # X Server e.g. VcXsrv
    # WSL1
    #export DISPLAY="localhost:0.0"
    # WSL2
    export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0
    # Emacs to fix for dbind-WARINING
    export NO_AT_BRIDGE=1
fi

# z
export Z=$HOME/distfiles/z/z.sh
if [ -f $Z ]; then
    source $Z
fi

export PATH=$HOME/.local/bin:$PATH
export LD_LIBRARY_PATH=$HOME/.local/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH

if [ -f "$HOME/.bash_local" ]; then
    source $HOME/.bash_local
fi
