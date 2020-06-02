set -gx ANDROID_HOME ~/Library/Android/sdk
set -gx ANDROID_SDK_ROOT ~/Library/Android/sdk
set -gx ANDROID_AVD_HOME ~/.android/avd
set -gx PATH ~/Library/Android/sdk/build-tools/29.0.1 $ANDROID_HOME/emulator $ANDROID_HOME/tools $PATH
set -gx PATH /usr/local/sbin /usr/local/bin $PATH

# add go binaries to path
set -gx GOPATH ~/go
set -gx PATH $GOPATH/bin $PATH

# GBA dev
set -gx DEVKITPRO /opt/devkitpro
set -gx DEVKITARM $DEVKITPRO/devkitARM
set -gx DEVKITPPC $DEVKITPRO/devkitPPC

set -gx PATH $DEVKITPRO/tools/bin $PATH


# colored man output
# from http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
setenv LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
setenv LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
setenv LESS_TERMCAP_me \e'[0m'           # end mode
setenv LESS_TERMCAP_se \e'[0m'           # end standout-mode
setenv LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
setenv LESS_TERMCAP_ue \e'[0m'           # end underline
setenv LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline

thefuck --alias | source

source ~/.cargo/env
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /Users/ghostpepper/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<
set -gx PATH /Users/ghostpepper/Documents/projects/massdns/scripts/ $PATH

# Base16 Shell
if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell/"
    source "$BASE16_SHELL/profile_helper.fish"
end
