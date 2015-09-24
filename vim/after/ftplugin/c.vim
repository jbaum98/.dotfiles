if !filereadable(expand("%:p:h")."/Makefile")
    setlocal makeprg=gcc\ -Wall\ -Wextra\ -o\ %<\ %
<<<<<<< HEAD
endif

nnoremap <leader>r :call RunFile()<cr>
nnoremap <leader>t :make \| call RunFile()<cr>

function RunFile()
    !./%:r
=======
    setlocal efm=%f:%l:%c:\ %trror:\ %m
    setlocal efm+=%f:%l:%c:\ %tarning:\ %m
    setlocal efm+=%-G%.%#
endif

nnoremap <leader>r :w \| make \| call RunFile()<cr>

function! RunFile()
    !./%:r 2>&1
>>>>>>> use after/ftplugin instead of autocmd
endfunction
