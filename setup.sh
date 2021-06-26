#!/bin/bash
script_dir=$(dirname $0 | xargs realpath)

zsh () {
  mkdir -p ~/.config
  ln -s $script_dir/.zshrc ~/.zshrc
  ln -s $script_dir/.config/zsh ~/.config

  ./emacs_setup.sh
}


if [ "$1" == "zsh" ]; then
  zsh
else
  echo Command unknown
  echo This script is built to setup config files for
  echo fish or zsh.
fi
