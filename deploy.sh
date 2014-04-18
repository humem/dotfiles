#!/bin/sh

# link configuration files to the home directory
FILES=".vimrc .gvimrc"
DIR=dotfiles
for f in $FILES
do
    ln -s -i $DIR/$f ..
done

# download neobundle plugin
BUNDLE=$HOME/.vim/bundle
NEOBUNDLE=$BUNDLE/neobundle.vim
if ! [ -d $NEOBUNDLE ]; then
    mkdir -p $BUNDLE
    git clone https://github.com/Shougo/neobundle.vim $NEOBUNDLE
fi
