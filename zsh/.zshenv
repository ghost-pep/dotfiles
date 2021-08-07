#!/usr/bin/env zsh

# make path unique
# zsh syncs PATH and the path list for us
typeset -U PATH path

# append
# path+=('/home/david/pear/bin')
# or prepend
# path=('/home/david/pear/bin' $path)

path+=("$HOME/.emacs.d/bin")
# So this is weird. In arch, we dynamically link haskell, but for development its easier to statically link
# I have removed this in order to let xmonad find the right ghc installation for dynamic linking
# If I want to do dev work, I have to manually call the staic ghc
# path=("$HOME/.ghcup/bin" $path)
path=("$HOME/.cabal/bin" $path)

# make it available duh
export PATH

# source secrets
source $HOME/.secrets

# use gpg-agent as ssh agent so I get nice pinentry rather than ssh text password entry
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
