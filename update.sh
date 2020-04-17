#!/bin/sh
pushd ~/.vim/pack/bundle
for dir in */*; do (cd "$dir" && echo $dir && git pull); done
popd
vim -c "helptags ALL" -c q


