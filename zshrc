export DOTFILE_DIR="$HOME/.dotfiles"
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=20000

export LANG=en_US.UTF-8

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

source $HOME/.zsh/zgen/zgen.zsh

if ! zgen saved; then
    echo "Creating a zgen save"

    zgen loadall <<EOPLUGINS
        robbyrussell/oh-my-zsh lib/
        zsh-users/zsh-syntax-highlighting
        zsh-users/zsh-history-substring-search
        zsh-users/zsh-completions
        srijanshetty/zsh-pandoc-completion
        robbyrussell/oh-my-zsh plugins/git
        robbyrussell/oh-my-zsh plugins/ssh-agent
        robbyrussell/oh-my-zsh plugins/rake
        robbyrussell/oh-my-zsh plugins/rbenv
        robbyrussell/oh-my-zsh plugins/common-aliases
        jbaum98/agnoster.zsh-theme agnoster.zsh-theme
EOPLUGINS

    zgen save
fi

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

source_all $DOTFILE_DIR/shell_startup

# start tmux
#if command -v tmux>/dev/null; then
#[[ ! $TERM =~ screen ]] && [ -z $TMUX ] && tmux -2 -u
#fi
