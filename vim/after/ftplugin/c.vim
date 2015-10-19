if !filereadable(expand("%:p:h")."/Makefile")
    setlocal makeprg=gcc\ -Wall\ -Wextra\ -o\ %<\ %
endif

setlocal efm=%f:%l:%c:\ %trror:\ %m
setlocal efm+=%f:%l:%c:\ %tarning:\ %m
setlocal efm+=%-G%.%#

nnoremap <leader>r :w \| make \| call RunFile()<cr>

function! RunFile()
    if !filereadable(expand("%:p:h")."/Makefile")
        !./%:r 2>&1
    else
        :make run
    end
endfunction
