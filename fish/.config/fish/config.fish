set -gx PATH /usr/local/sbin /usr/local/bin $PATH

# add go binaries to path
set -gx GOPATH ~/go
set -gx PATH $GOPATH/bin $PATH

# add ghci to path
set -gx PATH ~/.ghcup/bin $PATH

set -gx JAVA_HOME (/usr/libexec/java_home)

# colored man output
# from http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
setenv LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
setenv LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
setenv LESS_TERMCAP_me \e'[0m'           # end mode
setenv LESS_TERMCAP_se \e'[0m'           # end standout-mode
setenv LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
setenv LESS_TERMCAP_ue \e'[0m'           # end underline
setenv LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline

source ~/.cargo/env
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

set -gx PATH $PATH /opt/metasploit-framework/bin

fish_add_path ~/.emacs.d/bin

set -l SECRETSLOC ~/.config/fish/secrets.fish

if test -f $SECRETSLOC
   . $SECRETSLOC
end

starship init fish | source
