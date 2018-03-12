# -*- mode: sh; -*-

# load environment files
for file in $(find -L ~/.bash -name "*.bash" -type f); do
    source "$file"
done
