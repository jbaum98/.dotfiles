# -*- mode: sh; -*-

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

for config_file in $DOTFILES/lib/*; do
    source $config_file
done

_source_common "aliases" "environment"
_source_zsh "history" "misc" "packages"
