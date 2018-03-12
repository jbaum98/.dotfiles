# .zshenv is always sourced, define here exported variables that should
# be available to other programs.

# export VISUAL=subl
export EDITOR=nvim
export PAGER=less
#export PATH=$PATH:~/bin:/usr/local/bin

# load environment files
for file in $(find -L ~/.profile.d -name "*.env" -type f); do
    source $file
done

#export TERM="xterm-256color"
