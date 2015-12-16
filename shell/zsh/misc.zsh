
# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt extended_glob
setopt interactivecomments
skip_global_compinit=1

if [ `uname` = "Darwin" ]; then
    test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh
    fpath=(/usr/local/share/zsh-completions $fpath)
fi

autoload -U promptinit
promptinit
setopt prompt_subst

bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[1;5C' emacs-forward-word

fpath=($DOTFILE_DIR/lib $fpath)
autoload source_all get_ed
