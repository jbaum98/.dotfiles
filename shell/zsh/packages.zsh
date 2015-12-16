source "${DOTFILES}/shell/zsh/zgen/zgen.zsh"

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
        nojhan/liquidprompt
EOPLUGINS

    zgen save
fi
