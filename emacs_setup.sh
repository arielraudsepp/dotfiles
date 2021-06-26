#!/bin/bash
script_dir=$(dirname $0 | xargs realpath)

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
ln -s $script_dir/.doom.d ~/.doom.d
 ~/.emacs.d/bin/doom install
