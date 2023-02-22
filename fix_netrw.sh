#!/bin/bash

exe_path=`which nvim`
echo "exe:  $exe_path"
real_path=`readlink -f ${exe_path}`
echo "real: $real_path"
nvim_root="$(cd "$(dirname "$real_path")/.." && pwd)"
echo "root: $nvim_root"

if [ ! -f "${nvim_root}/share/nvim/runtime/autoload/netrw.vim.orig" ]; then
  patch -b -p0 -d ${nvim_root} -i "${PWD}/nvim/netrw.vim.patch"
fi
