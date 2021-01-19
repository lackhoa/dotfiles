# This file is executed once, when user logs in

export BASH_SILENCE_DEPRECATION_WARNING=1  # I know you like zsh, okay?

if [ -n "$BASH_VERSION" ]; then  # If running bash
    # Source ".bashrc" if exists,
    # because why wouldn't you want your login shell to be different from all your other shells?
    if [ -f "${HOME}/.bashrc" ]; then
        . "${HOME}/.bashrc"
    fi
fi

PATH+=":${HOME}/bin" # My custom scripts, which is source-controlled
PATH+=":${HOME}/.local/bin"
PATH+=":${HOME}/Library/Python/2.7/bin"

# Some stupid go and ksops stuff 
export XDG_CONFIG_HOME=$HOME/.config  # This one is a gnome config thing
export GOPATH=/Users/$USER/go
export PATH=$GOPATH/bin:$PATH
