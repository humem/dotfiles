#!/bin/sh

# $1: files to be linked
# $2: destination directory
# $3: relative path
link_files() {
    if [ $2 != "." ] && ! [ -d ../$2 ]; then
        mkdir -p ../$2
    fi
    for _f in $1
    do
        if ! [ -f ../$2/$_f ]; then
            echo "ln -s -i $3/dotfiles/$2/$_f ../$2"
            if [ $2 != "." ]; then
                ln -s -i $3/dotfiles/$2/$_f ../$2
            else
                ln -s -i $3/dotfiles/$_f ..
            fi
        fi
    done
}

# link configuration files to the home directory
link_files ".bashrc .dir_colors .gvimrc .inputrc .profile .tmux.conf .vimrc" . .

# for Emacs
link_files "init.el" .emacs.d ..
link_files "`ls .emacs.d/lisp`" .emacs.d/lisp ../..

# download neobundle plugin
bundle=$HOME/.vim/bundle
neobundle=$bundle/neobundle.vim
if ! [ -d $neobundle ]; then
    mkdir -p $bundle
    git clone https://github.com/Shougo/neobundle.vim $neobundle
fi
