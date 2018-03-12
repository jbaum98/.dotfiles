# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
# This was added in bash 4, so check if present
if shopt | grep -q globstar; then
    shopt -s globstar
fi
