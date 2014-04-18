#!/bin/sh

# link configuration files t
FILES=".vimrc .gvimrc"
DIR=dotfiles
for f in $FILES
do
    ln -s -i $DIR/$f ..
done

# download neobundle plugin
BUNDLE=$HOME/.vim/bundle
NEOBUNDLE=$BUNDLE/neobundle.
if ! [ -d $NEOBUNDLE ]; then
    mkdir -p $BUNDLE
    git clone https://github
fi
