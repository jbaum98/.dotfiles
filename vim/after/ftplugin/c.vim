if !filereadable(expand("%:p:h")."/Makefile")
    setlocal makeprg=gcc\ -Wall\ -Wextra\ -o\ %<\ %
    setlocal efm=%f:%l:%c:\ %trror:\ %m
    setlocal efm+=%f:%l:%c:\ %tarning:\ %m
    setlocal efm+=%-G%.%#
endif

nnoremap <leader>r :w \| make \| call RunFile()<cr>

function! RunFile()
    !./%:r 2>&1
endfunction
