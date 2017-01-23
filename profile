# -*- mode: sh; -*-

export DOTFILES="${HOME}/.dotfiles"

source $DOTFILES/lib/exists
source $DOTFILES/lib/add_path

case "$OSTYPE" in
    solaris*) OS="SOLARIS" ;;
    darwin*)  OS="OSX" ;; 
    linux*)   OS="LINUX" ;;
    bsd*)     OS="BSD" ;;
    *)        OS="unknown: $OSTYPE" ;;
esac


add_path "$HOME/.local/bin"
add_path "$HOME/.cabal/bin"
add_path "$HOME/bin"
add_path "$HOME/.cargo/bin"
if OS="OSX"; then
    add_path "/Applications/MATLAB_R2016b.app/bin"
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then 
    . "$HOME/.nix-profile/etc/profile.d/nix.sh";
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
