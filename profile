# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

autoload() {
    :
}

if [[ -L "${BASH_SOURCE[0]}" ]]; then
    DOTFILE_DIR="`readlink -e ${BASH_SOURCE[0]} | xargs dirname`"
else
    DOTFILE_DIR="`dirname ${BASH_SOURCE[0]}`"
fi

for f in $DOTFILE_DIR/lib/*; do
    source $f
done

source .source_startup

# vim:filetype=sh
