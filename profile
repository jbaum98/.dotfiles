export DOTFILES="${HOME}/.dotfiles"

source $DOTFILES/lib/exists

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

PATH=$PATH:$HOME/bin:$HOME/.cabal/bin

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
