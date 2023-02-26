#!/bin/bash

dots=(".dircolors" ".inputrc" ".tmux.conf" ".vimrc")
if [ -z "TERMUX_ENV" ]; then
  dots+=(".bash_aliases")
else
  ln -sf "${PWD}/.bash_aliases" "${HOME}/.bashrc"
fi
for f in "${dots[@]}"; do
  ln -sf "${PWD}/${f}" "${HOME}/"
done

mkdir -p "${HOME}/.emacs.d"
for f in init.el early-init.el; do
  ln -sf "${PWD}/.emacs.d/${f}" "${HOME}/.emacs.d/"
done

mkdir -p "${HOME}/.config/nvim"
ln -s "${PWD}/.config/nvim/init.lua" "${HOME}/.config/nvim/"

./fix_netrw.sh
