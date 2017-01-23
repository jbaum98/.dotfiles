source "${DOTFILES}/shell/zsh/zplug/init.zsh"

zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"

zplug "srijanshetty/zsh-pandoc-completion"

zplug "plugins/git", from:oh-my-zsh
zplug "plugins/ssh-agent", from:oh-my-zsh
zplug "plugins/rake", from:oh-my-zsh
zplug "plugins/rbenv", from:oh-my-zsh
zplug "plugins/common-aliases", from:oh-my-zsh

zplug "lib/clipboard", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"
zplug "lib/theme-and-appearance", from:oh-my-zsh

zplug "zsh-users/zsh-syntax-highlighting", defer:3

zplug "nojhan/liquidprompt"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

#setopt xtrace
# Then, source plugins and add commands to $PATH
zplug load
#unsetopt xtrace

#if ! zgen saved; then
#    echo "Creating a zgen save"
#
#    zgen loadall <<EOPLUGINS
#        robbyrussell/oh-my-zsh lib/
#        zsh-users/zsh-syntax-highlighting
#        zsh-users/zsh-history-substring-search
#        zsh-users/zsh-completions
#        zsh-users/zsh-autosuggestions
#        srijanshetty/zsh-pandoc-completion
#        nojhan/liquidprompt
#EOPLUGINS
#
#    zgen save
#fi
