# -*- mode: sh; -*-

export DOTFILES="${HOME}/.dotfiles"

source $DOTFILES/lib/exists

PATH=$HOME/.local/bin:$HOME/.cabal/bin:$HOME/bin:$PATH

if exists direnv; then
    if [ -n "$ZSH_VERSION" ]; then
        eval "$(direnv hook zsh)"
    else
        eval "$(direnv hook bash)"
    fi
fi

if exists hsvm; then
    eval "$(hsvm init)"
fi

# vim:filetype=sh
