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

# Adding all keys in my ssh directory to ssh agents (why wouldn't I want that? You think I have only one SSH key?)
SSH_DIR=${HOME}/.ssh
for identity in ${SSH_DIR}/*; do
    if [ -f ${identity} ] && [ ${identity} != "${SSH_DIR}/config" ] && [ ${identity} != "${SSH_DIR}/known_hosts" ]; then
        chmod 400 ${identity}
        ssh-add ${identity}
    fi
done
