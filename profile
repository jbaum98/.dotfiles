export DOTFILES="${HOME}/.dotfiles"

source $DOTFILES/lib/exists

PATH=$PATH:$HOME/bin:$HOME/.cabal/bin

if exists rbenv; then
    eval "$(rbenv init -)"
fi

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
