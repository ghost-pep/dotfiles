#!/usr/bin/env zsh

# make path unique
# zsh syncs PATH and the path list for us
typeset -U PATH path

# append
# path+=('/home/david/pear/bin')
# or prepend
# path=('/home/david/pear/bin' $path)

path+=("$HOME/.emacs.d/bin")

# make it available duh
export PATH

# source secrets
source $HOME/.secrets

# use gpg-agent as ssh agent so I get nice pinentry rather than ssh text password entry
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
