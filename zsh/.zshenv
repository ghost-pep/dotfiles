#!/usr/bin/env zsh

# make path unique
# zsh syncs PATH and the path list for us
typeset -U PATH path

# append
# path+=('/home/david/pear/bin')
# or prepend
# path=('/home/david/pear/bin' $path)

# make it available duh
export PATH
