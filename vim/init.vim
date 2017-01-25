" Jake Waksbaum

scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
if &compatible
    set nocompatible
endif
"  Dein {{{
set runtimepath+=~/.vim/bundle/repos/github.com/Shougo/dein.vim
if dein#load_state(expand('~/.vim/bundle'))
    call dein#begin('~/.vim/bundle')
    """ Let Dein manage itself
    call dein#add('Shougo/dein.vim')

    """ Color Schemes
    call dein#add('mnpk/vim-monokai')

    " Misc Utilities {{{
    call dein#add('vim-utils/vim-man')
    call dein#add('dhruvasagar/vim-markify')
    call dein#add('tpope/vim-speeddating')
    call dein#add('bling/vim-airline')
    call dein#add('vim-airline/vim-airline-themes')
    call dein#add('amiorin/vim-fenced-code-blocks')
    call dein#add('kien/ctrlp.vim')
    call dein#add('tacahiroy/ctrlp-funky')
    call dein#add('tpope/vim-fugitive')
    call dein#add('airblade/vim-gitgutter')
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-repeat')
    call dein#add('godlygeek/tabular')
    call dein#add('scrooloose/nerdcommenter')
    call dein#add('ap/vim-css-color')
    " }}}

    " Text Objects {{{
    " i,w is camelCase word
    call dein#add('bkad/CamelCaseMotion') 

    " aa and ia are arguments in a function call
    call dein#add('vim-scripts/argtextobj.vim') 

    call dein#add('kana/vim-textobj-user')

    " ar and ir are Ruby blocks
    call dein#add('nelstrom/vim-textobj-rubyblock') 
    " }}}

    " Languages {{{
    "" Markup Languages {{{{
    call dein#add('digitaltoad/vim-jade')     " Jade
    call dein#add('slim-template/vim-slim')   " Slim
    call dein#add('groenewege/vim-less')      " Less
    "" }}}}
    "" Configuration {{{{
    call dein#add('stephpy/vim-yaml')         " YAML
    call dein#add('tpope/vim-haml')           " HAML
    "" }}}}
    "" Javascript {{{{
    call dein#add('jelera/vim-javascript-syntax')
    call dein#add('pangloss/vim-javascript')
    call dein#add('crusoexia/vim-javascript-lib')
    "" }}}}
    call dein#add('rust-lang/rust.vim')       " Rust
    call dein#add('rhysd/vim-crystal')        " Crystal
    call dein#add('LnL7/vim-nix')             " Nix
    call dein#add('jceb/vim-orgmode')         " Orgmode
    call dein#add('kchmck/vim-coffee-script') " Coffee Script
    call dein#add('vim-ruby/vim-ruby')        " Ruby
    call dein#add('tpope/vim-rails')          " Rails
    call dein#add('dag/vim2hs')               " Haskell
    call dein#add('sophacles/vim-processing') " Processing
    " }}}
    "

    call dein#end()
    call dein#save_state()
endif
filetype plugin indent on    " required
syntax enable
" }}}

" Basics {{{
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
" use system clipboard
set clipboard=unnamed
let mapleader = ","
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

  "For certain filetypes, autoindent with two spaces, always expand tabs
  autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber,slim,crystal
              \ set ai sw=2 sts=2 et


  " Sass file recognition
  autocmd! BufRead,BufNewFile *.sass setfiletype sass

  " Leave the return key alone when in command line windows, since it's used
  " to run commands there.
  autocmd! CmdwinEnter * :unmap <cr>
  autocmd! CmdwinLeave * :call MapCR()
augroup END
" }}}

" Colors {{{
set number
let g:solarized_termcolors=256
set t_Co=256
set background=dark
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
colorscheme monokai
" }}}

" Spaces and Tabs {{{
set tabstop=4 " number of visual spaces per TAB
set softtabstop=4 " number of spaces in tab when editing
set shiftwidth=4
set expandtab " makes tabs spaces
set modelines=1
set backspace=indent,eol,start
set autoindent
set copyindent
set smartindent
" }}}

" Invisbles {{{ set list
set listchars=tab:▸\ ,trail:·
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
"if (exists('+colorcolumn'))
"    set colorcolumn=80
"    highlight ColorColumn ctermbg=9
"endif
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
"set guifont=Droid\ Sans\ Mono\ for\ Powerline:h15
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
"nnoremap <leader>T :call RunNearestTest()<cr>
"nnoremap <leader>t :call RunTestFile()<cr>
"nnoremap <leader>a :call RunTests('')<cr>
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
