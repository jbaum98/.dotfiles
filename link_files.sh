#!/bin/bash

REL_PATH="`dirname $0`"

# Execute this script from your home directory

main() {
     get_files | exclude_self | exclude_lib | compute_links | add_nvim_links | rm_existing_links | link_files
}

get_files() {
   ls $REL_PATH
}

exclude_self() {
    grep -v `basename $0`
}

exclude_lib() {
    grep -v "^lib$"
}

compute_links() {
    while read path; do
        echo "`src_path $path` `dest_path $path`"
    done
}

rm_existing_links() {
    while read link; do
        dest="`echo $link | cut -d ' ' -f 2`"
        if [ -L $dest ]; then
            rm $dest
        fi
        echo $link # to pass it on
    done
}

dest_path() {
    echo ".$1"
}

src_path() {
    echo "$REL_PATH/$1" 
}

link_files() {
    while read link; do link_file $link; done
}

link_file() {
    ln -s $1 $2
}

add_nvim_links() {
    while read link; do echo $link; done # pass on existing links
    echo ".vim .nvim"
    echo ".vimrc .nvimrc"
}

main
