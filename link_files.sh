#!/bin/bash

REL_PATH="`dirname $0`"
EXCLUDE=("`basename $0`" "lib" "shell_startup")

# Execute this script from your home directory

main() {
     get_files | exclude $EXCLUDE | compute_links | add_nvim_links | rm_existing_links | link_files
}

get_files() {
   for file in $REL_PATH/*; do
       basename "$file"
   done
}

exclude() {
    re="`join \| $*`"
    grep -Evx "$re"
}

join() { 
    local IFS="$1"
    shift
    echo "$*"
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
