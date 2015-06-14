" Jake Waksbaum
scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
" Plug {{{
"set nocompatible
call plug#begin('~/.vim/plugged')
" }}}

" Plugs {{{
Plug 'bling/vim-airline'
Plug 'Shougo/vimproc'
Plug 'Shougo/context_filetype.vim'
Plug 'mnpk/vim-monokai'
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'edsono/vim-matchit'
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'crusoexia/vim-javascript-lib'
Plug 'Raimondi/delimitMate'
Plug 'kchmck/vim-coffee-script'
Plug 'groenewege/vim-less'
Plug 'digitaltoad/vim-jade'
Plug 'mbbill/undotree'
Plug 'tpope/vim-surround'
Plug 'benekastah/neomake'
Plug 'tpope/vim-haml'
Plug 'vim-ruby/vim-ruby'
Plug 'slim-template/vim-slim'
Plug 'stephpy/vim-yaml'
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-rails'
Plug 'christoomey/vim-tmux-navigator'
Plug 'terryma/vim-multiple-cursors'
Plug 'mhinz/vim-startify'
Plug 'tacahiroy/ctrlp-funky'
Plug 'myusuf3/numbers.vim'
Plug 'godlygeek/tabular'
Plug 'ap/vim-css-color'
Plug 'spf13/PIV'
Plug 'Chiel92/vim-autoformat'
Plug 'lukerandall/haskellmode-vim'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
Plug 'sophacles/vim-processing'
call plug#end()
filetype plugin indent on    " required
" }}}

" Colors {{{
syntax enable
set number
set t_Co=256
set background=dark
colorscheme monokai
" }}}

" Spaces and Tabs {{{
set tabstop=4 " number of visual spaces per TAB
set softtabstop=4 " number of spaces in tab when editing
set shiftwidth=4
set expandtab " makes tabs spaces
filetype indent on
set modelines=1
set backspace=2
set smartindent
" }}}

" Invisbles {{{ set list
set listchars=tab:▸\ ,trail:·
hi NonText guifg=#444444¬
hi SpecialKey guifg=#444444
"}}}

" UI {{{
set number
set showcmd
set cursorline
set wildmenu
set showmatch
" }}}

" Search {{{
set incsearch
set hlsearch
" }}}

" Folding {{{
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent
" }}}

" Line Shortcuts {{{
nnoremap j gj
nnoremap k gk
nnoremap gV `[v`]
inoremap jk <esc>
" }}}

" Backups/Undo {{{
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup
nnoremap <leader>u :UndotreeToggle<cr>
if has("persistent_undo")
    set undodir='~/.undo/'
    set undofile
endif
" }}}

" Airline {{{
let g:airline_powerline_fonts = 0
let g:airline_theme = 'kalisi'
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set guifont=Droid\ Sans\ Mono\ for\ Powerline:h15
set encoding=utf-8
set fillchars+=stl:\ ,stlnc:\
set termencoding=utf-8
if has("gui_running")
    let s:uname = system("uname")
    if s:uname == "Darwin\n"
        set guifont=Droid\ Sans\ Mono\ for\ Powerline:h15
    endif
endif
" }}}

" Custom Mappings {{{
let mapleader = "-"
nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap H ^
nnoremap L $
inoremap <Left> <nop>
inoremap <Right> <nop>
inoremap <Up> <nop>
inoremap <Down> <nop>
nnoremap <Left> <nop>
nnoremap <Right> <nop>
nnoremap <Up> <nop>
nnoremap <Down> <nop>
inoremap <C-c> <CR><Esc>O
nnoremap <C-]> ^i<tab><Esc>
nnoremap <Leader>f :CtrlPFunky<Cr>
" }}}

" Filetypes {{{
autocmd Filetype java set makeprg=javac
autocmd FileType java let b:dispatch = 'java ' + expand('%:r')
autocmd Filetype ruby,yaml,html,php setlocal ts=2 sts=2 sw=2
" }}}

" NeoMake {{{
autocmd BufWritePost * Neomake
let g:neomake_java_enabled_makers=['javac']
" }}}

" Tmux {{{
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
" }}}

let g:formatprg_args_java = "--style=java"
"set clipboard=exclude:.*

autocmd InsertLeave,TextChanged * nested if expand('%') != '' | update | endif
nnoremap ; :
nnoremap : ;

" Haskell {{{
let g:haddock_browser="/usr/bin/env links2"
autocmd Filetype haskell set makeprg=ghc\ %
au BufEnter *.hs compiler ghc
let g:ghc="/usr/bin/env ghc"
" disable all conceals, including the simple ones like
" lambda and composition
let g:haskell_conceal              = 0
" disable concealing of 'enumerations': commatized lists like
" deriving clauses and LANGUAGE pragmas,
" otherwise collapsed into a single ellipsis
let g:haskell_conceal_enumerations = 0
let g:ghcmod_ghc_options = ['-Wall']
"}}}

" vim:foldmethod=marker:foldlevel=0
