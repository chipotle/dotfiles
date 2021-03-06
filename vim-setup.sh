#!/bin/sh
mkdir -p ~/.vim/swap
mkdir -p ~/.vim/pack/bundle/opt
mkdir -p ~/.vim/pack/bundle/start

pushd ~/.vim/pack/bundle/opt
git clone https://github.com/lifepillar/vim-solarized8
git clone https://github.com/rakr/vim-one
git clone https://github.com/morhetz/gruvbox 
git clone https://github.com/liuchengxu/space-vim-theme

cd ../start
git clone https://github.com/ctrlpvim/ctrlp.vim
git clone https://github.com/tpope/vim-fugitive
git clone https://github.com/tpope/vim-surround
git clone https://github.com/tpope/vim-endwise
git clone https://github.com/tpope/vim-rails
git clone https://github.com/editorconfig/editorconfig-vim
git clone https://github.com/rizzatti/dash.vim
git clone https://github.com/preservim/nerdtree

popd

cp update.sh ~/.vim

