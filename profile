# -*- mode: sh; -*-

export DOTFILES="${HOME}/.dotfiles"

source $DOTFILES/lib/exists
source $DOTFILES/lib/add_path

add_path "$HOME/.local/bin"
add_path "$HOME/.cabal/bin"
add_path "$HOME/bin"
add_path "$HOME/.cargo/bin"

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then 
    . /home/jake/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer

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
