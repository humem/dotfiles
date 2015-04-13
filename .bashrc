# Aliases
alias a='alias'
alias exi='exit'
alias g='grep'
alias l='less'
alias m='more'

if [ `uname -s` = 'Darwin' ]; then
    alias e='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs -nw'
    alias v='/Applications/MacVim.app/Contents/MacOS/Vim'
    alias ql='qlmanage -p'
fi

# Locale
export LANG=ja_JP.UTF-8
unset LC_CTYPE
alias en='unset LANG'
alias ja='export LANG=ja_JP.UTF-8'

# ls
export LSCOLORS=gxfxcxdxbxegedabagacad
alias la='ls -alFGv'
alias ll='ls -lFGv'
alias ls='ls -FGv'

# Python
export PYTHONDONTWRITEBYTECODE=1
alias i='ipython'

if [ -r $HOME/.bashrc_local ]; then source $HOME/.bashrc_local; fi
