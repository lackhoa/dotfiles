# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc) for examples
# This is executed for all interactive subshells, aka for each usage session

# I know you like zsh, okay?
export BASH_SILENCE_DEPRECATION_WARNING=1

# Annoying git log error in emacs terminal
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000
export EDITOR=emacs

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm|xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
	      # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	      # a case would tend to support setf rather than setaf.)
	      color_prompt=yes
    else
        color_prompt=
    fi
fi

# PS1 is a bash command, which is evaluated to become the prompt,
# so that it can change dynamically
# \h = host, \u -> user, w -> working directory
PS1="${debian_chroot}"
if [ "$color_prompt" = yes ]; then
    reset_color="\[\033[00m\]"
    user_color="\[\033[01;32m\]"
    host_color="\[\033[01;31m\]"
    dir_color="\[\033[01;33m\]"
    vc_color="\[\033[01;34m\]"
fi

if [[ ${EUID} != 0 ]] ; then  # EUID = effective user id
    PS1+="${user_color}\u"
fi
PS1+="@${host_color}\h "
PS1+="${dir_color}\w "
PS1+="${vc_color}\$(vcprompt)"  #note: We do not evaluate vcprompt here!
PS1+="\$"
PS1+=$reset_color
PS1+="\n"

unset color_prompt force_color_prompt

# If this is an xterm, set the window title to user@host:dir (MacOS should have taken care of it)
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot}\u@\h \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -x /usr/bin/mint-fortune ]; then
     /usr/bin/mint-fortune
fi

# Ignore case on completion
bind 'set completion-ignore-case on'

# Aliases, they are here so that typing "bash" inside your login shell will preserve the aliases
#note: do not use space anywhere in these!
alias cls='clear'
alias subl='/usr/bin/subl'
alias cp='cp -vi'
alias mv='mv -vi'
alias gs='git status'
alias gl='git log'
alias gps='git push'
alias gpl='git pull'
alias gc='git commit'
alias ga='git add'
alias gd='git diff'
alias gco='git checkout'
alias rm='rm -frv'
alias cat='bat'
alias tar='tar -xzvf'

alias k=kubectl
alias kus=kustomize
alias kctx='kubectl config current-context'
alias kc='kubectl config use-context'
alias ap='ansible-playbook'
alias tf='terraform'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Notify terminal about current context
if [ -n "${INSIDE_EMACS}" ]
then
    ansi_term_esc="\033AnSiT"
    printf ${ansi_term_esc}'h %s\n' $HOSTNAME
    printf ${ansi_term_esc}'u %s\n' $(whoami)
    printf ${ansi_term_esc}'c %s\n' $PWD
    function cd {
        command cd "$@"
        printf ${ansi_term_esc}'c %s\n' $PWD
    }
    function pushd {
        command pushd "$@"
        printf ${ansi_term_esc}'c %s\n' $PWD
    }
    function popd {
        command popd "$@"
        printf ${ansi_term_esc}'c %s\n' $PWD
    }
fi

# ls right after cd (note that cd has to succeed)
function cdl {
    cd "$@" && ls -F
}

# Working
alias work='source ~/.hatch-profile'
# Some stupid go and ksops stuff 
export XDG_CONFIG_HOME=$HOME/.config
export GOPATH=/Users/$USER/go
export PATH=$GOPATH/bin:$PATH
