" Jake Waksbaum

scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
" Plug {{{
call plug#begin('~/.vim/plugged')
" }}}

" Plugs {{{
"Plug 'mhinz/vim-startify'
Plug 'vim-utils/vim-man'
Plug 'dhruvasagar/vim-markify'
Plug 'jceb/vim-orgmode'
Plug 'tpope/vim-speeddating'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rhysd/vim-crystal'
Plug 'amiorin/vim-fenced-code-blocks'
"Plug 'vim-scripts/AnsiEsc.vim'
"Plug 'Shougo/vimproc'
"Plug 'Shougo/context_filetype.vim'
Plug 'mnpk/vim-monokai'
Plug 'jbaum98/vim-colors-solarized'
Plug 'kien/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
"Plug 'nathanaelkane/vim-indent-guides'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'godlygeek/tabular'
""" Text Objects
Plug 'bkad/CamelCaseMotion' " i,w is camelCase word
Plug 'vim-scripts/argtextobj.vim' " aa and ia are arguments in a function call
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock' " ar and ir are Ruby blocks
"""
""" Languages
"""""" Javascript
Plug 'jelera/vim-javascript-syntax'
Plug 'rhysd/vim-crystal'
Plug 'pangloss/vim-javascript'
Plug 'crusoexia/vim-javascript-lib'
Plug 'LnL7/vim-nix'
""""""
Plug 'kchmck/vim-coffee-script'
Plug 'digitaltoad/vim-jade'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'stephpy/vim-yaml'
Plug 'tpope/vim-haml'
Plug 'slim-template/vim-slim'
Plug 'groenewege/vim-less'
Plug 'dag/vim2hs'
Plug 'sophacles/vim-processing'
Plug 'rust-lang/rust.vim'
"""
Plug 'scrooloose/nerdcommenter'
"Plug 'christoomey/vim-tmux-navigator'
"Plug 'godlygeek/tabular'
Plug 'ap/vim-css-color'
"Plug 'spf13/PIV'
"Plug 'Chiel92/vim-autoformat'
call plug#end()
filetype plugin indent on    " required
" }}}

" Basics {{{
set nocompatible
" allow unsaved background buffers and remember marks/undo for them
set hidden
" remember more commands and search history
set history=10000
set cmdheight=1
set switchbuf=useopen
" This makes RVM work inside Vim. I have no idea why.
set shell=bash
" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=
" use emacs-style tab completion when selecting files, etc
set wildmode=longest,list
" make tab completion for files/buffers act like bash
set wildmenu
let mapleader = "\<Space>"
" Normally, Vim messes with iskeyword when you open a shell file. This can
" leak out, polluting other file types even after a 'set ft=' change. This
" variable prevents the iskeyword change so it can't hurt anyone.
let g:sh_noisk=1
" Insert only one space when joining lines that contain sentence-terminating
" punctuation like `.`.
set nojoinspaces
" If a file is changed outside of vim, automatically reload it without asking
set autoread
" }}}

" Autocmds {{{
augroup vimrcEx
  " Clear all autocmds in the group
  autocmd!
  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
              \ if line("'\"") > 0 && line("'\"") <= line("$") |
              \   exe "normal g`\"" |
              \ endif

  "for ruby, autoindent with two spaces, always expand tabs
  autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber,slim,crystal set ai sw=2 sts=2 et


  autocmd! BufRead,BufNewFile *.sass setfiletype sass
  " Leave the return key alone when in command line windows, since it's used
  " to run commands there.
  autocmd! CmdwinEnter * :unmap <cr>
  autocmd! CmdwinLeave * :call MapCR()
augroup END
" }}}

" Colors {{{
syntax enable
set number
let g:solarized_termcolors=256
set t_Co=256
set background=dark
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"autocmd VimEnter * colorscheme solarized
colorscheme monokai
" }}}

" Spaces and Tabs {{{
set tabstop=4 " number of visual spaces per TAB
set softtabstop=4 " number of spaces in tab when editing
set shiftwidth=4
set expandtab " makes tabs spaces
filetype plugin indent on
set modelines=1
set backspace=indent,eol,start
set autoindent
set copyindent
set smartindent
" }}}

" Invisbles {{{ set list
set listchars=tab:▸\ ,trail:·
"hi NonText guifg=#444444¬
"hi SpecialKey guifg=#444444
"}}}

" UI {{{
set showcmd
set cursorline
set wildmenu
set showmatch
set showtabline=2
set winwidth=79
" keep more context when scrolling off the end of a buffer
set scrolloff=3
if (exists('+colorcolumn'))
    set colorcolumn=80
    highlight ColorColumn ctermbg=9
endif
" }}}

" Git Gutter {{{
let g:gitgutter_enabled = 0
nnoremap <leader>gg :GitGutterToggle<cr>
nnoremap <leader>gr :GitGutterRevertHunk<cr>
nnoremap <leader>gn :GitGutterNextHunk<cr>
nnoremap <leader>gp :GitGutterPrevHunk<cr>
" }}}

" Search {{{
set incsearch
set hlsearch
" make searches case-sensitive only if they contain upper-case characters
set ignorecase smartcase
" }}}

" Folding {{{
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent
" }}}

" Backups/Undo {{{
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup
if has("persistent_undo")
    set undodir=~/.undo/
    set undofile
endif
" }}}

" Airline {{{
let g:airline_powerline_fonts = 0
let g:airline_theme = 'kalisi'
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set guifont=Droid\ Sans\ Mono\ for\ Powerline:h15
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
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
" Open and source .vimrc file
nnoremap <leader>erc :split $MYVIMRC<cr>
nnoremap <leader>src :source $MYVIMRC<cr>
nnoremap j gj
nnoremap k gk
" nnoremap gV `[v`]
inoremap fd <esc>
vnoremap fd <esc>
"Move splits more easily
nnoremap <leader>wj <c-w>j
nnoremap <leader>wk <c-w>k
nnoremap <leader>wh <c-w>h
nnoremap <leader>wl <c-w>l
" nnoremap H ^
" nnoremap L $
inoremap <C-c> <CR><Esc>O
nnoremap <C-]> ^i<tab><Esc>
nnoremap <Leader>f :CtrlPFunky<Cr>
nnoremap <Leader>p :CtrlP<Cr>
nnoremap <Leader>m :w \| make<cr><cr>
" Close all other windows, open a vertical split, and open this file's test
" alternate in it.
nnoremap <leader>s :call FocusOnFile()<cr>
function! FocusOnFile()
  tabnew %
  normal! v
  normal! l
  :A
  normal! h
endfunction
" Current directory
cnoremap <expr> %% expand('%:h').'/'
map <leader>e :edit %%
map <leader>v :view %%
" Rename current file
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>
" Promote to let
function! PromoteToLet()
  :normal! dd
  " :exec '?^\s*it\>'
  :normal! P
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
:command! PromoteToLet :call PromoteToLet()
:map <leader>L :PromoteToLet<cr>
" }}}

" Multipurpose Tab {{{
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>
" }}}

" Running Tests {{{
"function! MapCR()
  "nnoremap <cr> :call RunTestFile()<cr>
"endfunction
"call MapCR()
nnoremap <leader>T :call RunNearestTest()<cr>
nnoremap <leader>t :call RunTestFile()<cr>
nnoremap <leader>a :call RunTests('')<cr>
"nnoremap <leader>c :w\|:!script/features<cr>
"nnoremap <leader>w :w\|:!script/features --profile wip<cr>

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Run the tests for the previously-marked file.
    let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.py\)$') != -1
    if in_test_file
        call SetTestFile(command_suffix)
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file)
endfunction

function! RunNearestTest()
    let spec_line_number = line('.')
    call RunTestFile(":" . spec_line_number)
endfunction

function! SetTestFile(command_suffix)
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@% . a:command_suffix
endfunction

function! RunTests(filename)
    " Write the file and run tests for the given filename
    if expand("%") != ""
      :w
    end
    if match(a:filename, '\.feature$') != -1
        exec ":!script/features " . a:filename
    else
        " First choice: project-specific test script
        if filereadable("script/test")
            exec ":!script/test " . a:filename
        " Fall back to the .test-commands pipe if available, assuming someone
        " is reading the other side and running the commands
        elseif filewritable(".test-commands")
          let cmd = 'rspec --color --format progress --require "~/lib/vim_rspec_formatter" --format VimFormatter --out tmp/quickfix'
          exec ":!echo " . cmd . " " . a:filename . " > .test-commands"

          " Write an empty string to block until the command completes
          sleep 100m " milliseconds
          :!echo > .test-commands
          redraw!
        " Fall back to a blocking test run with Bundler
        elseif filereadable("Gemfile")
            exec ":!bundle exec rspec --color " . a:filename
        " If we see python-looking tests, assume they should be run with Nose
        elseif strlen(glob("test/**/*.py") . glob("tests/**/*.py"))
            exec "!nosetests " . a:filename
        " Fall back to a normal blocking test run
        else
            exec ":!rspec --color " . a:filename
        end
    end
endfunction
" }}}

" Markify {{{
let g:markify_error_text='✗✗'
let g:markify_warning_text='❢❢'
" }}}

" CtrlP {{{
let g:ctrlp_root_markers=['.project_root']
set wildignore+=*.o
" }}}

" vim:foldmethod=marker:foldlevel=0
