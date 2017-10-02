# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# MY DOINGS: Aliases:
alias cls='clear'
alias desktop='cdl ~/Desktop'
alias subl='/usr/bin/subl'
alias add-alias='vim ~/.profile'
alias update-alias='source ~/.profile'
alias cp='cp -vi'
alias mv='mv -vi'
alias cdw='cdl ~/Work'
alias cdd='cdl ~/Downloads'

# clean up stuff that were created less than 1 minute ago
alias clean='find . -type f -cmin -1 -delete'
alias conky-conf='sudo subl /etc/conky/conky.conf'

# this does everything I want to do about updating
# this command is stored in the root user's script path
alias khoa-update='sudo sh /usr/local/sbin/khoa-update'

#This command deals with updaing the hash of tex:
alias texhash='texhash & texhash ~/texmf'

#ls right after cd
function cdl {
    builtin cd "$@" && ls -F
    }
