if [ -n "$BASH_VERSION" ]; then  # if running bash
    if [ -f "$HOME/.bashrc" ]; then  # source ".bashrc" if it exists, why wouldn't you want your login shell to be different from your non-login shell?
        . "$HOME/.bashrc"
    fi
fi

# This is my custom scripts, which is source-controlled
export PATH="~/.local/bin:~/bin:$PATH"

### Following lines added by script ./init.sh on Wed Sep 23 12:08:01 EEST 2020:
export HATCH_ROOT='/home/khoa/hatch-ops'
alias switch='~/hatch-ops/configuration/switch-env.sh'
