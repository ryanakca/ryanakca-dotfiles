[ -f /etc/ksh.kshrc ] && . /etc/ksh.kshrc || true

export HISTFILE="$HOME/.ksh_history"
export HISTSIZE=5000

export VISUAL="vim"
export EDITOR="$VISUAL"
set -o emacs
