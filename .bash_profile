export BASH_SILENCE_DEPRECATION_WARNING=1  # I know you like zsh, okay?
if [ -n "$BASH_VERSION" ]; then  # if running bash
    if [ -f "${HOME}/.bashrc" ]; then  # source ".bashrc" if it exists, why wouldn't you want your login shell to be different from your non-login shell?
        . "${HOME}/.bashrc"
    fi
fi

# This is my custom scripts, which is source-controlled
PATH+=":${HOME}/.local/bin"
PATH+=":${HOME}/bin"
PATH+=":${HOME}/Library/Python/2.7/bin"
### Following lines added by script ./init.sh on Wed Sep 23 12:08:01 EEST 2020:
export HATCH_ROOT=${HOME}/hatch-ops
alias switch='. ${HOME}/hatch-ops/configuration/switch-env.sh'
