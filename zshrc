HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=20000

export TERM="$TERM-256color"
export LANG=en_US.UTF-8

source $HOME/.zsh/antigen/antigen.zsh

antigen bundles <<EOBUNDLES
                    robbyrussell/oh-my-zsh lib/
                    zsh-users/zsh-syntax-highlighting
                    zsh-users/zsh-history-substring-search
                    zsh-users/zsh-completions
                    git
                    ssh-agent
                    rake
                    rvm
                    rbenv
                    common-aliases
EOBUNDLES

antigen theme $HOME/.zsh agnoster_mod --no-local-clone

antigen apply

setopt extended_glob
setopt interactivecomments
skip_global_compinit=1

if [ `uname` = "Darwin" ]; then
    fpath=(/usr/local/share/zsh-completions $fpath)
else
    fpath=($HOME/.antigen/repos/https-COLON--SLASH--SLASH-github.com-SLASH-zsh-users-SLASH-zsh-completions.git/src $fpath)
fi

if hash direnv 2>/dev/null; then
    eval "$(direnv hook zsh)"
fi

if hash xiwi 2>/dev/null; then
    alias subl='LANG=en_US.UTF-8 xiwi subl -w'
fi


# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U promptinit
promptinit
setopt prompt_subst

alias tmux="tmux -2 -u"

alias brtest="brew install hello && brew test hello && brew rm hello"

alias be="bundle exec"
alias rspec='rspec --color --format documentation'

alias todos='grep -n "TODO" *'

alias et="emacsclient -a ''-t"
alias eg="xiwi emacsclient -a '' -c"
export EDITOR=vim

bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[1;5C' emacs-forward-word

# start tmux
#if command -v tmux>/dev/null; then
#[[ ! $TERM =~ screen ]] && [ -z $TMUX ] && tmux -2 -u
#fi

export PATH=$HOME/.cabal/bin:$PATH
eval "$(hsvm init)"
