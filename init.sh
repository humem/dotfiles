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

if [ ! -d "$HOME/.config/nvim/astronvim" ]; then
  git clone --depth 1 https://github.com/AstroNvim/AstroNvim ~/.config/nvim
fi
ln -sf "${PWD}/astronvim/user" "${HOME}/.config/nvim/lua/"
./fix_netrw.sh
nvim  --headless -c 'quitall'
