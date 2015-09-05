#!/bin/bash

REL_PATH="`dirname $0`"

# Execute this script from your home directory

main() {
     get_files | remove_self | link_files
     link_nvim "vim vimrc"
}

get_files() {
   ls $REL_PATH
}

remove_self() {
    grep -v `basename $0`
}

dest_path() {
    echo ".$1"
}

src_path() {
    echo "$REL_PATH/$1" 
}

link_files() {
    while read path; do link $path; done
}

link() {
    ln -s "`src_path $1`" "`dest_path $1`"
}

link_nvim() {
    for vim_file in $*; do 
        ln -s ".$vim_file" ".n$vim_file"
    done
}

main
