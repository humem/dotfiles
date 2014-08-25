#!/bin/sh

# link configuration files to the home directory
FILES=".vimrc .gvimrc .inputrc"
DEST_DIR=
SRC_DIR=dotfiles/$DEST_DIR
REL_DIR=
for f in $FILES
do
    if ! [ -f ../$DEST_DIR/$f ]; then
        ln -s -i $REL_DIR/$SRC_DIR/$f ../$DEST_DIR
    fi
done

# for Emacs
mkdir -p ../.emacs.d/lisp
FILES="init.el"
DEST_DIR=.emacs.d
SRC_DIR=dotfiles/$DEST_DIR
REL_DIR=../
for f in $FILES
do
    if ! [ -f ../$DEST_DIR/$f ]; then
        ln -s -i $REL_DIR$SRC_DIR/$f ../$DEST_DIR
    fi
done
FILES="my-powerline.el psvn.el"
DEST_DIR=.emacs.d/lisp
SRC_DIR=dotfiles/$DEST_DIR
REL_DIR=../../
for f in $FILES
do
    if ! [ -f ../$DEST_DIR/$f ]; then
        ln -s -i $REL_DIR$SRC_DIR/$f ../$DEST_DIR
    fi
done

# download neobundle plugin
BUNDLE=$HOME/.vim/bundle
NEOBUNDLE=$BUNDLE/neobundle.vim
if ! [ -d $NEOBUNDLE ]; then
    mkdir -p $BUNDLE
    git clone https://github.com/Shougo/neobundle.vim $NEOBUNDLE
fi
