setopt extended_glob
setopt interactivecomments
skip_global_compinit=1

autoload -U promptinit
promptinit
setopt prompt_subst

bindkey '^[[1;3C' forward-word
bindkey '^[f' forward-word
bindkey '^[[1;3D' backward-word
bindkey '^[b' backward-word
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[1;5C' emacs-forward-word
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

fpath=($DOTFILE_DIR/lib $fpath)
autoload source_all get_ed
