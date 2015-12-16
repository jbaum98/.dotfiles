# -*- mode: sh; -*-

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

export DOTFILES="$HOME/.dotfiles"

autoload() {
    typeset -fu
}

LIB="${DOTFILES}/lib/*"
for config_file in $LIB; do
    source $config_file
done

_source_common "aliases" "environment"
_source_bash   "history" "misc" "appearance"
