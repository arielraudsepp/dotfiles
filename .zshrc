case $(uname) in
  Linux)
    source $HOME/.config/zsh/os/linux.zsh
    ;;
  Darwin)
    source $HOME/.config/zsh/os/darwin.zsh
    ;;
  '*')
    echo Hi, stranger!
    ;;
esac

export DEFAULT_USER="Ariel";

# Add custom functions
source $HOME/.config/zsh/functions.zsh;

# Configuration specific to zinit
source $HOME/.config/zsh/zinit.zsh;

# Work stuff
#source $HOME/.config/zsh/secrets/work.zsh

# Personal Secrets
#source $HOME/.config/zsh/secrets/personal.zsh

# Track history
export HISTFILE=$HOME/.zsh_history;

# how many lines of history to keep in memory
HISTSIZE=10000;

# how many lines to keep in the history file
SAVEHIST=10000;

setopt hist_ignore_all_dups

eval $(starship init zsh)

# rust bins to PATH
export PATH="$HOME/.local/bin:$PATH:$HOME/bin:$HOME/.cargo/bin:$HOME/.emacs.d/bin"

# hook for direnv; leave at end of file
eval "$(direnv hook zsh)"

 # Nix
 if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
 fi
 # End Nix
