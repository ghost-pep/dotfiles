#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s autocd

alias ls='ls --group-directories-first --color=auto -F'
alias la='ls -la --group-directories-first --color=auto -F'
alias ll='ls -l --group-directories-first --color=auto -F'
alias grep='grep --color=auto'
alias cp='cp -i'
alias anime='~/bin/anime.sh'
alias music='~/bin/music.sh'
alias ida='wine /home/dayynn/.wine/drive_c/Program\ Files\ \(x86\)/IDA\ Free/idag'

#set variable in order to build android from cli
export ANDROID_HOME=/opt/android-sdk

GREEN="$(tput setaf 2)"
DRKBLUE="$(tput setaf 21)"
LTBLUE="$(tput setaf 111)"

RESET="$(tput sgr0)"
PS1='${DRKBLUE}[${LTBLUE}\A \W${DRKBLUE}]${RESET} > '
