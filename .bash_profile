# This is bash profile

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# Aliases (#note: do not use space anywhere):
alias cls='clear'
alias subl='/usr/bin/subl'
alias add-alias='emacs ~/.profile'
alias update-alias='source ~/.profile'
alias cp='cp -vi'
alias mv='mv -vi'
alias cdd='cdl ~/Downloads'
alias gs='git status'
alias gps='git push'
alias gpl='git pull'
alias gc='git commit'
alias ga='git add'
alias gd='git diff'
alias gco='git checkout'
alias rm='rm -rf'
alias cat='bat'
alias tar='tar -xzvf'
alias apt='sudo apt'
alias docker='sudo docker'
alias dockerd='sudo dockerd & disown'
alias terraform='sudo terraform'
alias conky-conf='sudo emacs /etc/conky/conky.conf'

# ls right after cd
function cdl {
    builtin cd "$@" && ls -F
}

# This is my local bin
export PATH="$PATH:~/bin"

### Following lines added by script ./init.sh on Wed Sep 23 12:08:01 EEST 2020:
export HATCH_ROOT='/home/khoa/hatch-ops'
alias switch='~/hatch-ops/configuration/switch-env.sh'

