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
        robbyrussell/oh-my-zsh plugins/rvm
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
    export RBENV_ROOT=/usr/local/var/rbenv
    fpath=(/usr/local/share/zsh-completions $fpath)
fi

if hash rbenv 2>/dev/null; then
    eval "$(rbenv init -)"
fi

if hash direnv 2>/dev/null; then
    eval "$(direnv hook zsh)"
fi

if hash xiwi 2>/dev/null; then
    alias subl='LANG=en_US.UTF-8 xiwi subl -w'
fi


if hash hsvm 2>/dev/null; then
    eval "$(hsvm init)"
fi

autoload -U promptinit
promptinit
setopt prompt_subst

alias tmux="tmux -2 -u"

alias brtest="brew install hello && brew test hello && brew rm hello"

alias be="bundle exec"

alias todos='grep -n "TODO" *'

#alias e="emacsclient -t -a \"\""
#alias E="SUDO_EDITOR=\"emacsclient -t -a \"\" \" sudoedit"
#alias eg="xiwi emacsclient -a \"\" -c"
alias e="nvim"
alias E="SUDO_EDITOR=\"nvim\" sudoedit"
export EDITOR=nvim

bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[1;5C' emacs-forward-word

# start tmux
#if command -v tmux>/dev/null; then
#[[ ! $TERM =~ screen ]] && [ -z $TMUX ] && tmux -2 -u
#fi

export PATH=$HOME/.cabal/bin:$PATH
