# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export DOTFILE_DIR="$HOME/.dotfiles"

if [ -n "$BASH_VERSION" ] && [ $0 == "-su" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

autoload() {
    :
}

source $DOTFILE_DIR/lib/source_all

source_all $DOTFILE_DIR/lib $DOTFILE_DIR/shell_startup
#source $DOTFILE_DIR/shell_startup/aliases
#source $DOTFILE_DIR/shell_startup/environment
#source $DOTFILE_DIR/shell_startup/index
#source $DOTFILE_DIR/shell_startup/path

# vim:filetype=sh
