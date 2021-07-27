#!/usr/bin/env zsh

# Path to your oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load
ZSH_THEME="robbyrussell"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="false"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=14

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# Use vim for quick edits and tools that rely on the EDITOR var
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh

# history config
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=4000

# needed settings
setopt autocd
unsetopt beep

# use emacs mode rather than vi mode for line editing
bindkey -e

# load completion and prompts
zstyle :compinstall filename '/home/ghostpepper/.zshrc'
autoload -Uz compinit promptinit
compinit
promptinit

# complete for my aliases
setopt COMPLETE_ALIASES

# fish like syntax highlighting
# relies on zsh-syntax-highlighting and zsh-autosuggestions packages
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# use gpg-agent as ssh agent so I get nice pinentry rather than ssh text password entry
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# Use pinentry with correct TTY
export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null
