#########################
# Aliases
#########################
alias ls="exa -lbh"

#########################
# Functions
#########################
emacs () {
    /usr/sbin/emacs $argv &
}

delete-merged () {
  git branch --merged \
    | egrep -v "(^\*|master|dev)" \
    | xargs git branch -d
}

# George Ornbo (shapeshed) http://shapeshed.com
# License - http://unlicense.org
#
# Fixes a corrupt .zsh_history file

clean_history() {
  mv ~/.zsh_history ~/.zsh_history_bad
  strings ~/.zsh_history_bad > ~/.zsh_history
  fc -R ~/.zsh_history
  rm ~/.zsh_history_bad
}
